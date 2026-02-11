// Prograrr - Frontend Application

// Track which seasons are expanded (survives re-renders)
const expandedSeasons = new Set();

// State
let state = {
  requests: [],
  loading: true,
  error: null,
  activeTab: 'downloading'
};

// API
async function fetchRequests() {
  try {
    const response = await fetch('/api/requests');
    if (!response.ok) throw new Error('Failed to fetch');
    const data = await response.json();
    state.requests = data;
    state.loading = false;
    state.error = null;
  } catch (err) {
    state.error = 'Failed to fetch requests';
    state.loading = false;
  }
  render();
}

// Helpers
function formatStatus(status) {
  const statusMap = {
    downloading: 'Downloading',
    seeding: 'Seeding',
    paused: 'Paused',
    queued: 'Queued',
    stalled: 'Stalled',
    completed: 'Completed'
  };
  return statusMap[status] || status;
}

function formatSpeed(bytesPerSec) {
  if (bytesPerSec < 1024) return `${bytesPerSec} B/s`;
  if (bytesPerSec < 1048576) return `${Math.round(bytesPerSec / 1024)} KB/s`;
  return `${(bytesPerSec / 1048576).toFixed(1)} MB/s`;
}

function formatEta(seconds) {
  if (seconds < 0) return '∞';
  if (seconds < 60) return `${seconds}s`;
  if (seconds < 3600) return `${Math.floor(seconds / 60)}m`;
  if (seconds < 86400) return `${Math.floor(seconds / 3600)}h ${Math.floor((seconds % 3600) / 60)}m`;
  return `${Math.floor(seconds / 86400)}d`;
}

function mediaTypeIcon(type) {
  return type === 'movie' ? '🎬' : '📺';
}

function formatQuality(quality) {
  // Extract just the resolution (e.g., "Bluray-1080p" -> "1080p")
  const match = quality.match(/(\d{3,4}p)/i);
  return match ? match[1] : quality;
}

// Pick the most relevant active download status across all episode downloads,
// falling back to the main request status. Prioritizes "downloading" over less
// active statuses like "stalled" so the badge reflects the best current state.
function bestDownloadStatus(request) {
  const priority = { downloading: 4, paused: 3, queued: 2, stalled: 1 };
  let best = null;
  let bestPri = -1;

  // Check episode downloads first
  if (request.episodeDownloads) {
    for (const ep of request.episodeDownloads) {
      const p = priority[ep.downloadStatus] ?? 0;
      if (p > bestPri) { best = ep.downloadStatus; bestPri = p; }
    }
  }

  // Check main request status
  if (request.downloadStatus && isActiveDownloadStatus(request.downloadStatus)) {
    const p = priority[request.downloadStatus] ?? 0;
    if (p > bestPri) { best = request.downloadStatus; }
  }

  return best || 'downloading';
}

// Tab filtering
function isActiveDownloadStatus(status) {
  return status === 'downloading' ||
         status === 'queued' ||
         status === 'paused' ||
         status === 'stalled';
}

function isActivelyDownloading(r) {
  if (isActiveDownloadStatus(r.downloadStatus)) return true;
  if (r.episodeDownloads && r.episodeDownloads.length > 0) {
    return r.episodeDownloads.some(ep => isActiveDownloadStatus(ep.downloadStatus));
  }
  return false;
}

function filterRequests(requests, tab) {
  switch (tab) {
    case 'downloading':
      return requests.filter(isActivelyDownloading);
    case 'missing':
      // Only show as missing if NOT actively downloading
      return requests.filter(r =>
        (r.isMissing || r.isNotAvailable) && !isActivelyDownloading(r)
      );
    case 'completed':
      // Only show as completed if NOT missing and NOT downloading
      return requests.filter(r =>
        !r.isMissing &&
        !r.isNotAvailable &&
        !isActivelyDownloading(r) &&
        (r.requestStatus === 'available' ||
         r.downloadStatus === 'seeding' ||
         r.downloadStatus === 'completed')
      );
    default:
      return requests;
  }
}

// Merge multiple requests for the same TV series into a single card.
// Jellyseerr creates separate requests per season, but we want one card per show.
function mergeSeriesRequests(requests) {
  const groups = new Map();
  const result = [];

  for (const r of requests) {
    if (r.mediaType !== 'tv') {
      result.push(r);
      continue;
    }
    const key = `${r.title}|${r.year || ''}`;
    if (!groups.has(key)) {
      groups.set(key, []);
    }
    groups.get(key).push(r);
  }

  for (const group of groups.values()) {
    if (group.length === 1) {
      result.push(group[0]);
      continue;
    }

    // Merge: use first request as base, combine episode downloads
    const base = group[0];
    const episodeMap = new Map();
    for (const r of group) {
      for (const ep of (r.episodeDownloads || [])) {
        const epKey = `${ep.seasonNumber}-${ep.episodeNumber}`;
        if (!episodeMap.has(epKey)) {
          episodeMap.set(epKey, ep);
        }
      }
    }
    const episodes = [...episodeMap.values()].sort((a, b) =>
      a.seasonNumber !== b.seasonNumber
        ? a.seasonNumber - b.seasonNumber
        : a.episodeNumber - b.episodeNumber
    );

    result.push({
      ...base,
      id: group.map(r => r.id).join('-'),
      episodeDownloads: episodes,
    });
  }

  return result;
}

function getTabCount(requests, tab) {
  return mergeSeriesRequests(filterRequests(requests, tab)).length;
}

function setActiveTab(tab) {
  state.activeTab = tab;
  render();
}

// Render
function render() {
  const app = document.getElementById('app');
  const filteredRequests = mergeSeriesRequests(filterRequests(state.requests, state.activeTab));

  app.innerHTML = `
    <div class="app">
      ${renderHeader()}
      <main class="main">
        ${state.error ? renderError() : ''}
        ${state.loading && state.requests.length === 0 ? renderLoading() : ''}
        ${!state.loading ? renderTabs() : ''}
        ${!state.loading && filteredRequests.length === 0 && !state.error ? renderEmpty() : ''}
        ${filteredRequests.length > 0 ? renderRequestsGrid(filteredRequests) : ''}
      </main>
    </div>
  `;

  // Attach tab click handlers
  document.querySelectorAll('.tab').forEach(tab => {
    tab.addEventListener('click', () => setActiveTab(tab.dataset.tab));
  });

  // Attach season header click handlers
  document.querySelectorAll('.season-header').forEach(header => {
    header.addEventListener('click', () => {
      const key = header.dataset.seasonKey;
      if (expandedSeasons.has(key)) {
        expandedSeasons.delete(key);
      } else {
        expandedSeasons.add(key);
      }
      render();
    });
  });
}

function renderHeader() {
  return `
    <header class="header">
      <div class="header-content">
        <h1 class="logo">Prograrr</h1>
        <p class="subtitle">Track your download progress</p>
      </div>
    </header>
  `;
}

function renderLoading() {
  return `
    <div class="loading">
      <div class="spinner"></div>
      <p>Loading requests...</p>
    </div>
  `;
}

function renderError() {
  return `
    <div class="error">
      <p>${state.error}</p>
    </div>
  `;
}

function renderEmpty() {
  return `
    <div class="empty">
      <p>No requests in this category</p>
    </div>
  `;
}

function renderTabs() {
  const tabs = [
    { id: 'downloading', label: 'Downloading' },
    { id: 'missing', label: 'Missing' },
    { id: 'completed', label: 'Completed' }
  ];

  return `
    <div class="tabs">
      ${tabs.map(tab => `
        <button
          class="tab ${state.activeTab === tab.id ? 'tab-active' : ''}"
          data-tab="${tab.id}"
        >
          ${tab.label}
          <span class="tab-count">${getTabCount(state.requests, tab.id)}</span>
        </button>
      `).join('')}
    </div>
  `;
}

function renderRequestsGrid(requests) {
  return `
    <div class="requests-grid">
      ${requests.map(renderRequestCard).join('')}
    </div>
  `;
}

function renderRequestCard(request) {
  return `
    <article class="request-card">
      ${renderPoster(request)}
      <div class="card-content">
        ${renderTitle(request)}
        ${renderBadges(request)}
        ${renderProgress(request)}
      </div>
    </article>
  `;
}

function renderPoster(request) {
  const content = request.posterUrl
    ? `<img src="${request.posterUrl}" alt="${request.title}" class="poster-img">`
    : `<div class="poster-placeholder">${mediaTypeIcon(request.mediaType)}</div>`;

  return `<div class="poster">${content}</div>`;
}

function renderTitle(request) {
  const year = request.year ? `<span class="year">(${request.year})</span>` : '';
  return `
    <div class="title-section">
      <h2 class="title">${request.title}</h2>
      ${year}
    </div>
  `;
}

function renderBadges(request) {
  const badges = [];

  // Actively downloading takes priority
  if (isActivelyDownloading(request)) {
    const status = bestDownloadStatus(request);
    badges.push(`<span class="badge badge-download badge-${status}">${formatStatus(status)}</span>`);
  } else if (request.isMissing) {
    // Missing (file was deleted)
    badges.push(`<span class="badge badge-missing">Missing</span>`);
  } else if (request.isNotAvailable) {
    // Not released yet
    badges.push(`<span class="badge badge-not-available">Not Available</span>`);
  } else if (request.downloadStatus) {
    // Completed/seeding
    badges.push(`<span class="badge badge-download badge-${request.downloadStatus}">${formatStatus(request.downloadStatus)}</span>`);
  }

  if (request.quality) {
    badges.push(`<span class="badge badge-quality">${formatQuality(request.quality)}</span>`);
  }

  if (request.requestedBy) {
    badges.push(`<span class="badge badge-user">${request.requestedBy}</span>`);
  }

  return `<div class="badges">${badges.join('')}</div>`;
}

function renderProgress(request) {
  // For TV shows with episode downloads, render per-episode progress
  if (request.mediaType === 'tv' && request.episodeDownloads && request.episodeDownloads.length > 0) {
    return renderEpisodeDownloads(request.id, request.episodeDownloads);
  }

  // Don't show progress bar if no progress, completed, or at 100%
  if (request.downloadProgress == null) return '';
  if (request.downloadProgress >= 100) return '';
  if (request.downloadStatus === 'seeding' || request.downloadStatus === 'completed') return '';

  const progress = request.downloadProgress.toFixed(1);
  const speed = request.downloadSpeed ? `<span class="download-speed">${formatSpeed(request.downloadSpeed)}</span>` : '';
  const eta = request.etaSeconds && request.etaSeconds > 0 && request.etaSeconds < 8640000
    ? `<span class="eta">${formatEta(request.etaSeconds)}</span>`
    : '';

  return `
    <div class="progress-section">
      <div class="progress-bar">
        <div class="progress-fill" style="width: ${progress}%"></div>
      </div>
      <div class="progress-info">
        <span class="progress-percent">${progress}%</span>
        ${speed}
        ${eta}
      </div>
    </div>
  `;
}

function renderEpisodeDownloads(requestId, episodes) {
  // Group episodes by season, filtering out completed/seeding
  const seasons = {};
  for (const ep of episodes) {
    if (ep.downloadStatus === 'seeding' || ep.downloadStatus === 'completed') continue;
    if (ep.downloadProgress != null && ep.downloadProgress >= 100) continue;
    const s = ep.seasonNumber;
    if (!seasons[s]) seasons[s] = [];
    seasons[s].push(ep);
  }

  const seasonKeys = Object.keys(seasons).sort((a, b) => Number(a) - Number(b));
  if (seasonKeys.length === 0) return '';

  return `
    <div class="episode-downloads">
      ${seasonKeys.map(s => {
        const eps = seasons[s];
        const key = `${requestId}-${s}`;
        const isExpanded = expandedSeasons.has(key);
        const avgProgress = eps.reduce((sum, ep) => sum + (ep.downloadProgress || 0), 0) / eps.length;
        const totalSpeed = eps.reduce((sum, ep) => sum + (ep.downloadSpeed || 0), 0);
        const maxEta = Math.max(...eps.map(ep => (ep.etaSeconds && ep.etaSeconds > 0 && ep.etaSeconds < 8640000) ? ep.etaSeconds : 0));

        const speedHtml = totalSpeed ? `<span class="download-speed">${formatSpeed(totalSpeed)}</span>` : '';
        const etaHtml = maxEta > 0 ? `<span class="eta">${formatEta(maxEta)}</span>` : '';

        const chevronSvg = `<svg class="season-chevron ${isExpanded ? 'season-chevron-expanded' : ''}" width="12" height="12" viewBox="0 0 12 12"><path d="M4 2l4 4-4 4" fill="none" stroke="currentColor" stroke-width="1.5" stroke-linecap="round" stroke-linejoin="round"/></svg>`;

        return `
          <div class="season-group">
            <div class="season-header" data-season-key="${key}">
              ${chevronSvg}
              <span class="season-label">Season ${s}</span>
              <span class="season-ep-count">${eps.length} ep${eps.length !== 1 ? 's' : ''}</span>
              <div class="season-summary-bar">
                <div class="progress-fill" style="width: ${avgProgress.toFixed(1)}%"></div>
              </div>
              <div class="episode-stats">
                <span class="progress-percent">${avgProgress.toFixed(1)}%</span>
                ${speedHtml}
                ${etaHtml}
              </div>
            </div>
            <div class="season-episodes ${isExpanded ? 'season-episodes-expanded' : ''}">
              <div class="season-episodes-inner">
                ${eps.map(renderEpisodeProgress).join('')}
              </div>
            </div>
          </div>
        `;
      }).join('')}
    </div>
  `;
}

function renderEpisodeProgress(ep) {
  const progress = ep.downloadProgress != null ? ep.downloadProgress.toFixed(1) : '0.0';
  const label = ep.episodeNumber === 0
    ? 'Season Pack'
    : `E${String(ep.episodeNumber).padStart(2, '0')}${ep.episodeTitle ? ' - ' + ep.episodeTitle : ''}`;

  const speed = ep.downloadSpeed ? `<span class="download-speed">${formatSpeed(ep.downloadSpeed)}</span>` : '';
  const eta = ep.etaSeconds && ep.etaSeconds > 0 && ep.etaSeconds < 8640000
    ? `<span class="eta">${formatEta(ep.etaSeconds)}</span>`
    : '';

  return `
    <div class="episode-row">
      <div class="episode-header">
        <span class="episode-label">${label}</span>
        <div class="episode-stats">
          <span class="progress-percent">${progress}%</span>
          ${speed}
          ${eta}
        </div>
      </div>
      <div class="progress-bar-sm">
        <div class="progress-fill" style="width: ${progress}%"></div>
      </div>
    </div>
  `;
}

// Init
function init() {
  render();
  fetchRequests();
  // Poll every 15 seconds to reduce API load
  setInterval(fetchRequests, 15000);
}

document.addEventListener('DOMContentLoaded', init);
