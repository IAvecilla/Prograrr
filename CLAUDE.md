# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Build and Development Commands

```sh
gleam build          # Compile the project
gleam run            # Run the server (requires env vars)
gleam test           # Run tests
gleam deps download  # Download dependencies
```

## Required Environment Variables

The server requires these API keys:
- `JELLYSEERR_API_KEY`
- `SONARR_API_KEY`
- `RADARR_API_KEY`

Optional (with defaults):
- `PORT` (default: 3000)
- `JELLYSEERR_URL` (default: http://localhost:5055)
- `SONARR_URL` (default: http://localhost:8989)
- `RADARR_URL` (default: http://localhost:7878)
- `QBITTORRENT_URL` (default: http://localhost:8080)
- `QBITTORRENT_USERNAME` (default: admin)
- `QBITTORRENT_PASSWORD` (default: adminadmin)

## Architecture Overview

Pulsearr is a Gleam web service that aggregates media request data from multiple sources into a unified API. It runs on the Erlang/OTP runtime using Wisp (web framework) and Mist (HTTP server).

### Data Flow

1. **Jellyseerr** → Source of truth for media requests (movies/TV shows)
2. **Sonarr/Radarr** → Download queue status, provides TMDB/TVDB IDs for matching
3. **qBittorrent** → Real-time torrent progress (requires session cookie auth)

The aggregator (`src/api/aggregator.gleam`) combines data from all sources by:
- Fetching requests from Jellyseerr
- Matching to Sonarr queue items by TVDB ID (TV shows)
- Matching to Radarr queue items by TMDB ID (movies)
- Matching torrents by download hash or fuzzy name search

### Module Structure

- `src/pulsearr.gleam` - Entry point, server setup
- `src/router.gleam` - HTTP routing, serves API and static files
- `src/config.gleam` - Environment variable loading
- `src/api/aggregator.gleam` - Core business logic, combines all data sources
- `src/api/jellyseerr.gleam` - Jellyseerr API client with request enrichment
- `src/api/arr.gleam` - Sonarr/Radarr queue API client (shared implementation)
- `src/api/qbittorrent.gleam` - qBittorrent API client with cookie auth
- `src/api/http_client.gleam` - HTTP utilities wrapper around gleam_httpc
- `src/models/request.gleam` - Type definitions and JSON encoders

### API Endpoints

- `GET /api/requests` - Returns aggregated media requests with download status
- `GET /api/health` - Health check
- `GET /api/debug` - Debug info from all services
- `/*` - Serves static SPA frontend from `./static/`
