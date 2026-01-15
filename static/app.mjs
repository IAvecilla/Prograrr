// Prograrr - Frontend Application

// State
let state = {
  requests: [],
  loading: true,
  error: null
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
    pending: 'Pending',
    approved: 'Approved',
    available: 'Available',
    processing: 'Processing',
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

// Render
function render() {
  const app = document.getElementById('app');
  app.innerHTML = `
    <div class="app">
      ${renderHeader()}
      <main class="main">
        ${state.error ? renderError() : ''}
        ${state.loading && state.requests.length === 0 ? renderLoading() : ''}
        ${!state.loading && state.requests.length === 0 && !state.error ? renderEmpty() : ''}
        ${state.requests.length > 0 ? renderRequests() : ''}
      </main>
    </div>
  `;
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
      <p>No requests found</p>
    </div>
  `;
}

function renderRequests() {
  return `
    <div class="requests-grid">
      ${state.requests.map(renderRequestCard).join('')}
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
  const badges = [
    `<span class="badge badge-${request.requestStatus}">${formatStatus(request.requestStatus)}</span>`
  ];

  if (request.downloadStatus) {
    badges.push(`<span class="badge badge-download badge-${request.downloadStatus}">${formatStatus(request.downloadStatus)}</span>`);
  }

  if (request.requestedBy) {
    badges.push(`<span class="badge badge-user">${request.requestedBy}</span>`);
  }

  return `<div class="badges">${badges.join('')}</div>`;
}

function renderProgress(request) {
  if (request.downloadProgress == null) return '';

  const progress = request.downloadProgress.toFixed(1);
  const speed = request.downloadSpeed ? `<span class="download-speed">${formatSpeed(request.downloadSpeed)}</span>` : '';
  const eta = request.etaSeconds ? `<span class="eta">${formatEta(request.etaSeconds)}</span>` : '';

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
