// SPDX-License-Identifier: AGPL-3.0-or-later

export interface Config {
	token: string;
	baseUrl: string;
	spotify: {clientId: string; clientSecret: string} | null;
	ytDlpBin: string;
	cookiesFile: string;
	healthPort: number;
}

function required(env: NodeJS.ProcessEnv, name: string): string {
	const value = env[name];
	if (!value) throw new Error(`Missing required environment variable ${name}`);
	return value;
}

export function loadConfig(env: NodeJS.ProcessEnv): Config {
	const clientId = env.SPOTIFY_CLIENT_ID;
	const clientSecret = env.SPOTIFY_CLIENT_SECRET;
	return {
		token: required(env, 'FLUXER_MUSIC_BOT_TOKEN'),
		baseUrl: required(env, 'FLUXER_BASE_URL'),
		spotify: clientId && clientSecret ? {clientId, clientSecret} : null,
		ytDlpBin: env.YTDLP_BIN || 'yt-dlp',
		cookiesFile: env.YTDLP_COOKIES || '/run/music-secrets/yt-cookies.txt',
		healthPort: Number(env.HEALTH_PORT) || 8099,
	};
}
