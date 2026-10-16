# Build stage
FROM ghcr.io/gleam-lang/gleam:v1.14.0-erlang-alpine AS builder

WORKDIR /build

COPY gleam.toml manifest.toml ./
RUN gleam deps download

COPY src/ src/
RUN gleam export erlang-shipment

# Runtime stage: same base image so the Erlang/OTP version matches the build
FROM ghcr.io/gleam-lang/gleam:v1.14.0-erlang-alpine

WORKDIR /app

RUN adduser -D appuser

COPY --from=builder --chown=appuser /build/build/erlang-shipment /app
COPY --chown=appuser static/ /app/static/

ENV PORT=3000
EXPOSE 3000

USER appuser

CMD ["/app/entrypoint.sh", "run"]
