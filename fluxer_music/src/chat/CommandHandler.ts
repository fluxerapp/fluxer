// SPDX-License-Identifier: AGPL-3.0-or-later

import type {LoopMode} from '@fluxer/music_protocol/src/Types';
import {MusicError} from '../Errors';
import type {FluxerRest} from '../fluxer/FluxerRest';
import {VoiceJoinError} from '../fluxer/GatewayClient';
import type {VoiceStates} from '../fluxer/VoiceStates';
import type {MessageWire, ReactionWire} from '../fluxer/Wire';
import type {ChannelSession} from '../session/ChannelSession';
import type {SessionManager} from '../session/SessionManager';
import {formatDuration} from './LiveCard';
import {type Command, parseCommand} from './ParseCommand';

export interface CommandHandlerDeps {
	sessions: SessionManager;
	voiceStates: VoiceStates;
	rest: FluxerRest;
	botUserId: () => string | null;
	botSessionId: () => string | null;
}

const PAGE_SIZE = 10;
const GUILD_VOICE = 2;
const P2P_CODES = new Set(['P2P_GRANT', 'VOICE_P2P_CONSENT_REQUIRED', 'VOICE_P2P_UNAVAILABLE']);
const QUEUE_LIMIT = 500;
const VARIATION_SELECTOR = '️';
const NEXT_LOOP: Record<LoopMode, LoopMode> = {off: 'track', track: 'queue', queue: 'off'};

const HELP = [
	'Commands:',
	'!play <link or search> — add a track or playlist',
	'!playnext <link or search> — add it to play next',
	'!skip — skip the current track',
	'!pause — pause playback',
	'!resume — resume playback',
	'!np — show the now playing card',
	'!queue [page] — list the queue',
	'!remove <position> — remove a queued track',
	'!move <from> <to> — move a queued track',
	'!clear — clear the queue',
	'!shuffle — shuffle the queue',
	'!loop <off|track|queue> — set the loop mode',
	'!seek <m:ss | h:mm:ss> — jump within the track',
	'!stop — stop and leave',
	'!help — show this message',
].join('\n');

function describeError(err: unknown): string {
	return err instanceof Error ? err.message : String(err);
}

export class CommandHandler {
	/** Channel id → channel type. A channel's type never changes. */
	private readonly channelTypes = new Map<string, number>();

	constructor(private readonly deps: CommandHandlerDeps) {}

	async onMessageCreate(msg: MessageWire): Promise<void> {
		if (msg.author.bot) return;
		if (msg.guild_id === undefined) return;
		const command = parseCommand(msg.content);
		if (!command) return;
		// Commands only work in the chat of a guild voice channel.
		if (!(await this.isVoiceChannel(msg.channel_id))) return;

		if (command.name === 'help') return this.reply(msg, HELP);
		if (command.name === 'invalid') return this.reply(msg, `Usage: ${command.usage}`);
		if (!this.deps.voiceStates.isIn(msg.author.id, msg.channel_id)) {
			return this.reply(msg, 'Join the voice channel first.');
		}
		try {
			const text = await this.run(msg, msg.guild_id, command);
			if (text) await this.reply(msg, text);
		} catch (err) {
			await this.reply(msg, this.errorText(err));
		}
	}

	async onReactionAdd(r: ReactionWire): Promise<void> {
		if (r.user_id === this.deps.botUserId()) return;
		const session = this.deps.sessions.get(r.channel_id);
		if (!session || r.message_id !== session.cardMessageId) return;

		if (this.deps.voiceStates.isIn(r.user_id, r.channel_id)) {
			try {
				await this.react(session, r.user_id, r.emoji.name.replaceAll(VARIATION_SELECTOR, ''));
			} catch (err) {
				console.error(`reaction action failed: ${describeError(err)}`);
			}
		}
		try {
			await this.deps.rest.removeUserReaction(
				r.channel_id,
				r.message_id,
				r.emoji.name,
				r.user_id,
				this.deps.botSessionId() ?? undefined,
			);
		} catch (err) {
			console.error(`reaction removal failed: ${describeError(err)}`);
		}
	}

	private async isVoiceChannel(channelId: string): Promise<boolean> {
		let type = this.channelTypes.get(channelId);
		if (type === undefined) {
			try {
				type = (await this.deps.rest.getChannel(channelId)).type;
			} catch (err) {
				console.error(`channel lookup failed: ${describeError(err)}`);
				return false;
			}
			this.channelTypes.set(channelId, type);
		}
		return type === GUILD_VOICE;
	}

	private async react(session: ChannelSession, by: string, emoji: string): Promise<void> {
		switch (emoji) {
			case '⏯': {
				const status = session.state().status;
				if (status === 'playing' || status === 'loading') session.pause(by);
				else if (status === 'paused') session.resume(by);
				return;
			}
			case '⏭':
				session.skip(by);
				return;
			case '🔀':
				session.shuffle(by);
				return;
			case '🔁':
				session.setLoop(by, NEXT_LOOP[session.state().loop]);
				return;
			case '⏹':
				await session.stop(by);
				return;
		}
	}

	/** Runs a command and returns the reply text, if any. */
	private async run(msg: MessageWire, guildId: string, command: Command): Promise<string | null> {
		const by = msg.author.id;
		if (command.name === 'play' || command.name === 'playnext') {
			const session = await this.deps.sessions.getOrStart(guildId, msg.channel_id);
			await session.add(by, command.arg, command.name === 'playnext');
			return null;
		}
		const session = this.deps.sessions.get(msg.channel_id);
		if (!session) return 'Nothing is playing.';

		switch (command.name) {
			case 'skip':
				session.skip(by);
				return null;
			case 'pause':
				session.pause(by);
				return null;
			case 'resume':
				session.resume(by);
				return null;
			case 'np':
				await session.repostCard();
				return null;
			case 'queue':
				return this.queueText(session, command.page);
			case 'remove': {
				const track = session.queuePage(0, QUEUE_LIMIT).tracks[command.index - 1];
				if (!track) throw new MusicError('NOT_FOUND');
				session.remove(by, track.id);
				return null;
			}
			case 'move': {
				const {tracks} = session.queuePage(0, QUEUE_LIMIT);
				const track = tracks[command.from - 1];
				if (!track || command.to > tracks.length) throw new MusicError('NOT_FOUND');
				session.move(by, track.id, command.to - 1);
				return null;
			}
			case 'clear':
				session.clear(by);
				return null;
			case 'shuffle':
				session.shuffle(by);
				return null;
			case 'loop':
				session.setLoop(by, command.mode);
				return null;
			case 'seek':
				session.seek(by, command.ms);
				return null;
			case 'stop':
				await session.stop(by);
				return null;
			case 'help':
			case 'invalid':
				return null;
		}
	}

	private queueText(session: ChannelSession, requestedPage: number): string {
		const {tracks, total} = session.queuePage(0, QUEUE_LIMIT);
		if (total === 0) return 'Queue is empty.';
		const pages = Math.ceil(total / PAGE_SIZE);
		const page = Math.min(requestedPage, pages);
		const start = (page - 1) * PAGE_SIZE;
		const lines = tracks
			.slice(start, start + PAGE_SIZE)
			.map((t, i) => `${start + i + 1}. ${t.title} — ${t.author} (${formatDuration(t.durationMs)})`);
		return [`Queue (page ${page}/${pages}):`, ...lines].join('\n');
	}

	private errorText(err: unknown): string {
		if (err instanceof MusicError) return err.message;
		if (err instanceof VoiceJoinError) {
			if (P2P_CODES.has(err.code)) return "Music needs a channel that isn't peer-to-peer.";
			if (err.code === 'E2EE') return "Music doesn't work in end-to-end encrypted voice channels yet.";
			return `Couldn't join the voice channel (${err.code}).`;
		}
		console.error(`command failed: ${describeError(err)}`);
		return 'Something went wrong.';
	}

	private async reply(msg: MessageWire, content: string): Promise<void> {
		try {
			await this.deps.rest.sendMessage(msg.channel_id, {content, message_reference: {message_id: msg.id}});
		} catch (err) {
			console.error(`reply failed: ${describeError(err)}`);
		}
	}
}
