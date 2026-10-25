// SPDX-License-Identifier: AGPL-3.0-or-later

import type {VoiceStates} from '../fluxer/VoiceStates';
import type {VoiceStateWire} from '../fluxer/Wire';
import type {ChannelSession} from './ChannelSession';

/** Channel id → session. Starts sessions on demand and ends them when the channel empties or the bot is dropped. */
export class SessionManager {
	private readonly sessions = new Map<string, ChannelSession>();
	private readonly starting = new Map<string, Promise<ChannelSession>>();

	constructor(
		private readonly factory: (guildId: string, channelId: string) => ChannelSession,
		private readonly voiceStates: VoiceStates,
		private readonly isBot: (userId: string) => boolean,
	) {}

	get(channelId: string): ChannelSession | undefined {
		const session = this.sessions.get(channelId);
		return session && !session.ended ? session : undefined;
	}

	getOrStart(guildId: string, channelId: string): Promise<ChannelSession> {
		const existing = this.get(channelId);
		if (existing) return Promise.resolve(existing);
		const pending = this.starting.get(channelId);
		if (pending) return pending;

		const session = this.factory(guildId, channelId);
		session.once('ended', () => {
			if (this.sessions.get(channelId) === session) this.sessions.delete(channelId);
		});
		const start = session.start().then(
			() => {
				this.starting.delete(channelId);
				if (!session.ended) this.sessions.set(channelId, session);
				return session;
			},
			(err: unknown) => {
				this.starting.delete(channelId);
				throw err;
			},
		);
		this.starting.set(channelId, start);
		return start;
	}

	/** Call after `voiceStates.apply(state)`, so membership already reflects the change. */
	onVoiceState(state: VoiceStateWire, botUserId: string): void {
		if (state.user_id === botUserId) {
			for (const session of this.sessions.values()) {
				if (session.connectionId === state.connection_id && state.channel_id !== session.channelId) {
					this.endSession(session, 'dropped');
				}
			}
			return;
		}
		// A leave has no channel id, so re-check every session in the guild the state belongs to.
		if (state.guild_id === null) return;
		for (const session of this.sessions.values()) {
			if (session.guildId !== state.guild_id) continue;
			const humans = [...this.voiceStates.usersIn(session.channelId)].filter((id) => !this.isBot(id));
			if (humans.length === 0) this.endSession(session, 'empty');
		}
	}

	async endAll(): Promise<void> {
		await Promise.allSettled(this.starting.values());
		await Promise.all([...this.sessions.values()].map((s) => s.end('stop')));
		this.sessions.clear();
	}

	private endSession(session: ChannelSession, reason: 'dropped' | 'empty'): void {
		this.sessions.delete(session.channelId);
		session.end(reason).catch((err) => console.error(`ending session failed: ${String(err)}`));
	}
}
