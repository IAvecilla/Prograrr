# Prograrr

A lightweight dashboard that aggregates and displays real-time download progress from your *arr stack (Sonarr, Radarr) combined with Jellyseerr requests and qBittorrent/SABnzbd status.

![Prograrr Dashboard](https://img.shields.io/badge/docker-ignacioavecilla%2Fprograrr-blue)

![Prograrr Screenshot](screenshot.png)

## Features

- Unified view of all media requests from Jellyseerr
- Real-time download progress from Sonarr/Radarr queues
- Torrent status and speed from qBittorrent
- Usenet download progress from SABnzbd

## How It Works

Prograrr aggregates data from multiple services to provide a complete view of your media downloads:

| Service | Data Provided |
|---------|---------------|
| **Jellyseerr** | Requests list, requester, request status, media metadata (title, poster, TMDB/TVDB IDs) |
| **Sonarr** | TV show download queue, TVDB ID, download hash |
| **Radarr** | Movie download queue, TMDB ID, download hash |
| **qBittorrent** | Download progress %, speed, ETA, torrent state |
| **SABnzbd** | Usenet download progress %, speed, ETA, state |

**Data flow:**
```
Jellyseerr (request info: title, poster, who requested)
    ↓ matched by TMDB/TVDB ID
Sonarr/Radarr (queue item with download hash)
    ↓ matched by hash
qBittorrent / SABnzbd (real-time progress, speed, ETA)
```

Jellyseerr, Sonarr/Radarr, and at least one download client (qBittorrent or SABnzbd) are required to display complete information. SABnzbd is optional and can be used alongside qBittorrent.

## Quick Start

```bash
docker run -d \
  --name prograrr \
  -p 3000:3000 \
  -e PROGRARR_API_KEY=your_prograrr_api_key \
  -e JELLYSEERR_URL=http://jellyseerr:5055 \
  -e JELLYSEERR_API_KEY=your_jellyseerr_api_key \
  -e SONARR_URL=http://sonarr:8989 \
  -e SONARR_API_KEY=your_sonarr_api_key \
  -e RADARR_URL=http://radarr:7878 \
  -e RADARR_API_KEY=your_radarr_api_key \
  -e QBITTORRENT_URL=http://qbittorrent:8080 \
  -e QBITTORRENT_USERNAME=your_username \
  -e QBITTORRENT_PASSWORD=your_password \
  -e SABNZBD_URL=http://sabnzbd:8080 \
  -e SABNZBD_API_KEY=your_sabnzbd_api_key \
  ignacioavecilla/prograrr:latest
```

## Docker Compose Integration

Add Prograrr to your existing *arr stack `docker-compose.yml`:

```yaml
services:
  # ... your existing services (sonarr, radarr, jellyseerr, etc.)

  prograrr:
    image: ignacioavecilla/prograrr:latest
    container_name: prograrr
    environment:
      - TZ=America/New_York
      - PORT=3000
      - PROGRARR_API_KEY=${PROGRARR_API_KEY}
      - JELLYSEERR_URL=http://jellyseerr:5055
      - JELLYSEERR_API_KEY=${JELLYSEERR_API_KEY}
      - SONARR_URL=http://sonarr:8989
      - SONARR_API_KEY=${SONARR_API_KEY}
      - RADARR_URL=http://radarr:7878
      - RADARR_API_KEY=${RADARR_API_KEY}
      - QBITTORRENT_URL=http://qbittorrent:8080
      - QBITTORRENT_USERNAME=${QBITTORRENT_USERNAME}
      - QBITTORRENT_PASSWORD=${QBITTORRENT_PASSWORD}
      - SABNZBD_URL=http://sabnzbd:8080
      - SABNZBD_API_KEY=${SABNZBD_API_KEY}
    ports:
      - "3000:3000"
    restart: unless-stopped
```

### Using with VPN Container (Gluetun)

If your *arr apps run through a VPN container like Gluetun, point the URLs to the VPN container:

```yaml
  prograrr:
    image: ignacioavecilla/prograrr:latest
    container_name: prograrr
    environment:
      - PROGRARR_API_KEY=${PROGRARR_API_KEY}
      - JELLYSEERR_URL=http://jellyseerr:5055
      - JELLYSEERR_API_KEY=${JELLYSEERR_API_KEY}
      - SONARR_URL=http://gluetun:8989
      - SONARR_API_KEY=${SONARR_API_KEY}
      - RADARR_URL=http://gluetun:7878
      - RADARR_API_KEY=${RADARR_API_KEY}
      - QBITTORRENT_URL=http://gluetun:8080
      - QBITTORRENT_USERNAME=${QBITTORRENT_USERNAME}
      - QBITTORRENT_PASSWORD=${QBITTORRENT_PASSWORD}
    ports:
      - "3000:3000"
    restart: unless-stopped
    networks:
      - your_media_network

networks:
  your_media_network:
    external: true
```

## Environment Variables

| Variable | Required | Default | Description |
|----------|----------|---------|-------------|
| `PROGRARR_API_KEY` | **Yes** | - | API key to protect all routes (all API requests require `X-Api-Key` header) |
| `JELLYSEERR_API_KEY` | **Yes** | - | Jellyseerr API key |
| `SONARR_API_KEY` | **Yes** | - | Sonarr API key |
| `RADARR_API_KEY` | **Yes** | - | Radarr API key |
| `QBITTORRENT_USERNAME` | **Yes** | - | qBittorrent username |
| `QBITTORRENT_PASSWORD` | **Yes** | - | qBittorrent password |
| `PORT` | No | `3000` | Port the web server listens on |
| `JELLYSEERR_URL` | No | `http://localhost:5055` | Jellyseerr instance URL |
| `SONARR_URL` | No | `http://localhost:8989` | Sonarr instance URL |
| `RADARR_URL` | No | `http://localhost:7878` | Radarr instance URL |
| `QBITTORRENT_URL` | No | `http://localhost:8080` | qBittorrent Web UI URL |
| `SABNZBD_URL` | No | `http://localhost:8080` | SABnzbd URL |
| `SABNZBD_API_KEY` | No | - | SABnzbd API key (leave empty to disable) |
| `CORS_ORIGIN` | No | `*` (all origins) | Restrict CORS to a specific origin (e.g. `http://localhost:3000`) |
| `TZ` | No | `UTC` | Timezone |

## Getting API Keys

### Jellyseerr
1. Open Jellyseerr and go to **Settings**
2. Click on **General**
3. Copy the **API Key** or generate a new one

### Sonarr
1. Open Sonarr and go to **Settings** > **General**
2. Under **Security**, copy the **API Key**

### Radarr
1. Open Radarr and go to **Settings** > **General**
2. Under **Security**, copy the **API Key**

### qBittorrent
1. Open qBittorrent and go to **Options** > **Web UI**
2. Ensure **Web User Interface** is enabled
3. Note the username and password configured

### SABnzbd
1. Open SABnzbd and go to **Config** > **General**
2. Copy the **API Key** (full access)

## API Endpoints

All API endpoints except `/api/health` require the `X-Api-Key` header matching your `PROGRARR_API_KEY`.

| Endpoint | Description |
|----------|-------------|
| `GET /api/requests` | Returns aggregated media requests with download status. Supports `?tmdbId=` filter |
| `GET /api/overview` | Returns processed, ready-to-render data split into downloading and missing categories |
| `GET /api/health` | Health check endpoint |
| `GET /api/debug` | Debug info showing connection status to all services |

### `GET /api/overview`

A processed endpoint designed for client integrations (e.g. Jellyfin plugins). Returns two categories *downloading* (active downloads) and *missing* (not found / not available), with TV shows merged into single entries and episodes grouped by season.

Key behaviors:
- **Movies** have top-level `progress`/`speed`/`eta`/`quality` fields; `seasons` is `[]`
- **TV shows** have per-season aggregates (avg progress, total speed, max ETA) with per-episode detail; top-level progress fields are `null`
- Multiple Jellyseerr requests for different seasons of the same series are merged into one entry
- Completed/seeding episodes are filtered out, only active downloads are shown.
- `effectiveStatus` reflects the best status across all episodes (`downloading` > `paused` > `queued` > `stalled`)

Response shape:

```json
{
  "downloading": [
    {
      "title": "The Simpsons",
      "mediaType": "tv",
      "posterUrl": "https://image.tmdb.org/t/p/w500/...",
      "year": 1989,
      "requestedBy": "nacho",
      "effectiveStatus": "downloading",
      "progress": null,
      "speed": null,
      "eta": null,
      "quality": null,
      "seasons": [
        {
          "seasonNumber": 4,
          "episodeCount": 2,
          "progress": 45.5,
          "speed": 5242880,
          "eta": 3600,
          "episodes": [
            {
              "episodeNumber": 3,
              "title": "Homer the Heretic",
              "progress": 45.5,
              "speed": 5242880,
              "eta": 3600,
              "status": "downloading",
              "quality": "1080p"
            }
          ]
        }
      ]
    },
    {
      "title": "Sinners",
      "mediaType": "movie",
      "posterUrl": "...",
      "year": 2025,
      "requestedBy": "nacho",
      "effectiveStatus": "downloading",
      "progress": 72.3,
      "speed": 3145728,
      "eta": 900,
      "quality": "1080p",
      "seasons": []
    }
  ],
  "missing": [
    {
      "title": "Send Help",
      "mediaType": "movie",
      "posterUrl": "...",
      "year": 2026,
      "requestedBy": "nacho",
      "effectiveStatus": null,
      "progress": null,
      "speed": null,
      "eta": null,
      "quality": null,
      "seasons": []
    }
  ]
}
```

## Security

Prograrr is designed to never expose your credentials:

- **No credentials in API responses** — API keys and passwords are only used server-side to connect to your services. They are never included in any JSON response.
- **Mandatory authentication** — All API data endpoints require a valid `X-Api-Key` header. The app will not start without `PROGRARR_API_KEY` set.
- **Constant-time key comparison** — API key validation uses `gleam/crypto.secure_compare` to prevent timing-based attacks.
- **Brute force protection** — Failed authentication attempts are delayed by 1 second to limit brute force to ~1 attempt/second.
- **No secrets in error messages** — Error messages strip query parameters from URLs to prevent leaking credentials (e.g. SABnzbd API keys passed via URL).
- **No default credentials** — All sensitive values (`PROGRARR_API_KEY`, `QBITTORRENT_USERNAME`, `QBITTORRENT_PASSWORD`, and all API keys) must be explicitly configured. Empty values are rejected.
- **Configurable CORS** — Set `CORS_ORIGIN` to restrict which origins can call the API. Defaults to `*` (all origins).

## Development

Built with [Gleam](https://gleam.run/) using:
- **Wisp** - Web framework
- **Mist** - HTTP server

### Using Nix (Recommended)

The project includes a Nix flake for easy development setup:

```bash
# Clone the repository
git clone https://github.com/ignacioavecilla/prograrr.git
cd prograrr

# Enter the dev shell (installs Gleam, Erlang, etc.)
nix develop

# Or if you use direnv, it will activate automatically
direnv allow
```

### Manual Setup

```bash
# Install Gleam and Erlang manually, then:
gleam deps download

# Run locally (set environment variables first)
export PROGRARR_API_KEY=your_key
export JELLYSEERR_API_KEY=your_key
export SONARR_API_KEY=your_key
export RADARR_API_KEY=your_key
export QBITTORRENT_USERNAME=your_username
export QBITTORRENT_PASSWORD=your_password
gleam run

# Build Docker image
docker build -t prograrr .
```
