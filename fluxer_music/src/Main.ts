// SPDX-License-Identifier: AGPL-3.0-or-later

import {Player} from './audio/Player';
import {RoomLink} from './audio/RoomLink';
import {loadConfig} from './Config';
import {CommandHandler} from './chat/CommandHandler';
import {LiveCard} from './chat/LiveCard';
import {createDispatchHandler} from './Dispatch';
import {FluxerRest} from './fluxer/FluxerRest';
import {GatewayClient} from './fluxer/GatewayClient';
import {VoiceStates} from './fluxer/VoiceStates';
import {startHealthServer} from './Health';
import {Resolver} from './resolve/Resolver';
import {SpotifyClient} from './resolve/Spotify';
import {YouTubeResolver} from './resolve/YouTube';
import {createYtDlpRunner} from './resolve/YtDlp';
import {ChannelSession} from './session/ChannelSession';
import {SessionManager} from './session/SessionManager';

const DEFAULT_BITRATE = 64_000;
const SHUTDOWN_MS = 10_000;

function log(message: string): void {
	console.log(`[music] ${message}`);
}

function describeError(err: unknown): string {
	return err instanceof Error ? err.message : String(err);
}

async function main(): Promise<void> {
	const config = loadConfig(process.env);

	const rest = new FluxerRest({baseUrl: config.baseUrl, token: config.token});
	await rest.discover();
	const gateway = new GatewayClient({token: config.token, url: await rest.gatewayUrl()});

	const voiceStates = new VoiceStates();
	const bots = new Set<string>();

	const resolver = new Resolver(
		new YouTubeResolver(createYtDlpRunner({bin: config.ytDlpBin, cookiesFile: config.cookiesFile})),
		config.spotify ? new SpotifyClient(config.spotify) : null,
	);

	const createSession = (guildId: string, channelId: string): ChannelSession => {
		let bitrate = DEFAULT_BITRATE;
		return new ChannelSession(guildId, channelId, {
			resolver,
			createPlayer: (sink) => new Player(sink),
			room: new RoomLink({bitrate: () => bitrate}),
			voice: {
				join: async () => {
					bitrate = await rest
						.getChannel(channelId)
						.then((channel) => channel.bitrate ?? DEFAULT_BITRATE)
						.catch(() => DEFAULT_BITRATE);
					return gateway.joinVoice(guildId, channelId);
				},
				refresh: (connectionId) => gateway.refreshVoice(guildId, channelId, connectionId),
				leave: (connectionId) => gateway.leaveVoice(guildId, connectionId),
			},
			card: new LiveCard(channelId, rest),
			now: Date.now,
		});
	};

	const sessions = new SessionManager(createSession, voiceStates, (userId) => bots.has(userId));
	const commands = new CommandHandler({
		sessions,
		voiceStates,
		rest,
		botUserId: () => gateway.userId,
		botSessionId: () => gateway.sessionId,
	});

	gateway.on('dispatch', createDispatchHandler({voiceStates, sessions, commands, gateway, bots}));

	gateway.on('fatal', (code: number) => {
		console.error(`[music] gateway closed with fatal code ${code}`);
		process.exit(1);
	});

	const health = startHealthServer(config.healthPort, () => gateway.ready);

	let shuttingDown = false;
	const shutdown = (signal: string): void => {
		if (shuttingDown) return;
		shuttingDown = true;
		log(`${signal} received, shutting down`);
		const deadline = new Promise<void>((resolve) => setTimeout(resolve, SHUTDOWN_MS).unref());
		Promise.race([sessions.endAll(), deadline])
			.catch((err) => console.error(`[music] shutdown failed: ${describeError(err)}`))
			.finally(() => {
				gateway.close();
				health.close();
				process.exit(0);
			});
	};
	process.on('SIGTERM', () => shutdown('SIGTERM'));
	process.on('SIGINT', () => shutdown('SIGINT'));

	await gateway.connect();
	log('connected to the gateway');
}

process.on('unhandledRejection', (reason) => {
	console.error(`[music] unhandled rejection: ${describeError(reason)}`);
});

main().catch((err) => {
	console.error(`[music] startup failed: ${describeError(err)}`);
	process.exit(1);
});
