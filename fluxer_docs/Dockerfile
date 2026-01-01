FROM node:24-bookworm-slim AS base
WORKDIR /usr/src/app
RUN apt-get update && apt-get install -y --no-install-recommends libvips libvips-dev curl openssl && rm -rf /var/lib/apt/lists/*
RUN corepack enable

FROM base AS deps
WORKDIR /usr/src/app
COPY package.json pnpm-lock.yaml ./
COPY source.config.ts ./
RUN pnpm install --frozen-lockfile

FROM base AS build
WORKDIR /usr/src/app
COPY --from=deps /usr/src/app/node_modules ./node_modules
COPY . ./
RUN pnpm run build

FROM base AS runner
WORKDIR /usr/src/app
ENV NODE_ENV=production
ENV PORT=3000
COPY --from=build /usr/src/app/.next ./.next
COPY --from=build /usr/src/app/public ./public
COPY --from=build /usr/src/app/package.json ./package.json
COPY --from=deps /usr/src/app/node_modules ./node_modules
USER nobody
EXPOSE 3000
CMD ["node", "node_modules/next/dist/bin/next", "start", "-p", "3000"]
