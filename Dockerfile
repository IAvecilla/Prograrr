# Build stage
FROM ghcr.io/gleam-lang/gleam:v1.14.0-erlang-alpine AS builder

WORKDIR /app

# Copy project files
COPY gleam.toml .
COPY src/ src/
COPY test/ test/

# Download dependencies and build
RUN gleam deps download && gleam build

# Runtime stage - use the gleam image since it has gleam installed
FROM ghcr.io/gleam-lang/gleam:v1.14.0-erlang-alpine

WORKDIR /app

# Copy entire project from builder
COPY --from=builder /app /app

# Copy static files (JavaScript frontend)
COPY static/ /app/static/

# Set environment defaults
ENV PORT=3000
ENV JELLYSEERR_URL=http://localhost:5055
ENV SONARR_URL=http://localhost:8989
ENV RADARR_URL=http://localhost:7878
ENV QBITTORRENT_URL=http://localhost:8080

EXPOSE 3000

# Run the application using gleam run
CMD ["gleam", "run"]
