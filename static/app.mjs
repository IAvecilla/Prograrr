// Prograrr - Frontend Application

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

// Tab filtering
function isActivelyDownloading(r) {
  return r.downloadStatus === 'downloading' ||
         r.downloadStatus === 'queued' ||
         r.downloadStatus === 'paused' ||
         r.downloadStatus === 'stalled';
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

function getTabCount(requests, tab) {
  return filterRequests(requests, tab).length;
}

function setActiveTab(tab) {
  state.activeTab = tab;
  render();
}

// Render
function render() {
  const app = document.getElementById('app');
  const filteredRequests = filterRequests(state.requests, state.activeTab);

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
    badges.push(`<span class="badge badge-download badge-${request.downloadStatus}">${formatStatus(request.downloadStatus)}</span>`);
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

// Init
function init() {
  render();
  fetchRequests();
  // Poll every 15 seconds to reduce API load
  setInterval(fetchRequests, 15000);
}

document.addEventListener('DOMContentLoaded', init);
