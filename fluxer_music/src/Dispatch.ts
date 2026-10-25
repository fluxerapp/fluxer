// SPDX-License-Identifier: AGPL-3.0-or-later

import type {CommandHandler} from './chat/CommandHandler';
import type {VoiceStates} from './fluxer/VoiceStates';
import type {MessageWire, ReactionWire, VoiceStateWire} from './fluxer/Wire';
import type {SessionManager} from './session/SessionManager';

/** A voice state as the gateway sends it; `member.user.bot` says whether the participant is a bot. */
type VoiceStateDispatch = VoiceStateWire & {member?: {user?: {bot?: boolean}} | null};
type GuildCreateDispatch = {id: string; voice_states?: Array<VoiceStateDispatch>};

export interface DispatchDeps {
	voiceStates: VoiceStates;
	sessions: SessionManager;
	commands: CommandHandler;
	gateway: {readonly userId: string | null; leaveVoice(guildId: string, connectionId: string): void};
	/** User ids seen as bots in voice states; filled in here. */
	bots: Set<string>;
}

function describeError(err: unknown): string {
	return err instanceof Error ? err.message : String(err);
}

/** Routes gateway dispatches. A handler that throws or rejects is logged, never thrown out to the gateway. */
export function createDispatchHandler(deps: DispatchDeps): (type: string, d: unknown) => void {
	const {voiceStates, sessions, commands, gateway, bots} = deps;
	const noteBot = (state: VoiceStateDispatch): void => {
		if (state.member?.user?.bot) bots.add(state.user_id);
	};
	const guard = (what: string, work: Promise<void>): void => {
		work.catch((err) => console.error(`[music] ${what} failed: ${describeError(err)}`));
	};

	const route = (type: string, d: unknown): void => {
		switch (type) {
			case 'GUILD_CREATE': {
				const guild = d as GuildCreateDispatch;
				const states = (guild.voice_states ?? []).map((s) => ({...s, guild_id: s.guild_id ?? guild.id}));
				for (const state of states) noteBot(state);
				voiceStates.applyGuild(guild.id, states);
				// Connections left over from before a restart; skip the ones a live session owns.
				const botId = gateway.userId;
				if (botId === null) return;
				for (const conn of voiceStates.connectionsOf(botId)) {
					if (sessions.get(conn.channelId)?.connectionId === conn.connectionId) continue;
					gateway.leaveVoice(conn.guildId, conn.connectionId);
				}
				return;
			}
			case 'VOICE_STATE_UPDATE': {
				const state = d as VoiceStateDispatch;
				noteBot(state);
				voiceStates.apply(state);
				const botId = gateway.userId;
				if (botId !== null) sessions.onVoiceState(state, botId);
				return;
			}
			case 'MESSAGE_CREATE':
				guard('message handler', commands.onMessageCreate(d as MessageWire));
				return;
			case 'MESSAGE_REACTION_ADD':
				guard('reaction handler', commands.onReactionAdd(d as ReactionWire));
				return;
		}
	};

	return (type, d) => {
		try {
			route(type, d);
		} catch (err) {
			console.error(`[music] ${type} handler failed: ${describeError(err)}`);
		}
	};
}
