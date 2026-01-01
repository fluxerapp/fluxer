.PHONY: help build-caddy build up down restart logs clean

help:
	@echo "Available commands:"
	@echo "  make build-caddy  - Build the Caddy Docker image"
	@echo "  make build        - Build all images (including Caddy)"
	@echo "  make up           - Start all services"
	@echo "  make down         - Stop all services"
	@echo "  make restart      - Restart all services"
	@echo "  make logs         - View logs from all services"
	@echo "  make logs-caddy   - View Caddy logs"
	@echo "  make logs-livekit - View LiveKit logs"
	@echo "  make clean        - Stop services and remove volumes"

build-caddy:
	docker build -f Dockerfile.caddy -t fluxer-livekit-caddy:latest .

build: build-caddy
	docker compose build

up:
	docker compose up -d

down:
	docker compose down

restart:
	docker compose restart

logs:
	docker compose logs -f

logs-caddy:
	docker compose logs -f caddy

logs-livekit:
	docker compose logs -f livekit

clean:
	docker compose down -v
