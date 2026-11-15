// SPDX-License-Identifier: AGPL-3.0-or-later

import {setTimeout as sleep} from 'node:timers/promises';
import {applyPauseGuildInvites, type GuildInvitePauseDeps} from '@app/api/guild/services/GuildInvitePauseApplier';
import {emitActivity} from '@app/api/infrastructure/activity/ActivityEvents';
import {workerMeta} from '@app/api/infrastructure/activity/ActivityMeta';
import {
	ACTION_PARTITIONS,
	ACTIONS_STREAM,
	type ActionEnvelope,
	type ActionOutcome,
	CONTRACT_VERSION,
	effectsConsumer,
	MAX_ACTION_ATTEMPTS,
} from '@app/api/infrastructure/activity/Contract.generated';
import {Logger} from '@app/api/Logger';
import {
	type AccountStateDeps,
	applyDeleteUserMessages,
	applyHideProfile,
	applyHideRecentMessages,
	applyLimitNewConversations,
	applySetAccountLimit,
	applyTempBanIp,
	outcomeOf,
} from '@app/api/user/services/AccountStateApplier';
import {
	AckPolicy,
	type ConsumerMessages,
	JetStreamApiCodes,
	JetStreamApiError,
	type JetStreamClient,
	type JsMsg,
} from '@nats-io/jetstream';

export interface AccountActionDeps {
	js: Pick<JetStreamClient, 'consumers' | 'streams'>;
	state: AccountStateDeps;
	guildInvites: GuildInvitePauseDeps;
	publishOutcome?: (key: string, outcome: ActionOutcome) => Promise<void>;
	now?: () => number;
	retryDelayMs?: number;
}

const RETRY_DELAY_MS = 5000;
const STREAM_RECHECK_MS = 300_000;
const MAX_NAK_DELAY_MS = 30_000;

interface ConsumerState {
	stopped: boolean;
	abort: AbortController;
	loops: Array<ConsumerMessages>;
	tasks: Array<Promise<void>>;
	streamReady: Promise<void> | null;
}

let running: ConsumerState | null = null;

function decodeActionEnvelope(data: Uint8Array | string): ActionEnvelope | null {
	try {
		const value = JSON.parse(typeof data === 'string' ? data : new TextDecoder().decode(data)) as unknown;
		if (
			typeof value === 'object' &&
			value !== null &&
			typeof (value as ActionEnvelope).id === 'string' &&
			typeof (value as ActionEnvelope).key === 'string' &&
			typeof (value as ActionEnvelope).type === 'string' &&
			typeof (value as ActionEnvelope).v === 'number' &&
			typeof (value as ActionEnvelope).expires_at_ms === 'number'
		) {
			return value as ActionEnvelope;
		}
	} catch {
		return null;
	}
	return null;
}

export function applyAction(deps: AccountActionDeps, env: ActionEnvelope): Promise<ActionOutcome> {
	if (env.v !== CONTRACT_VERSION) return Promise.resolve(outcomeOf(env, 'unsupported'));
	if (env.expires_at_ms <= (deps.now?.() ?? Date.now())) return Promise.resolve(outcomeOf(env, 'expired'));
	switch (env.type) {
		case 'set_account_limit':
			return applySetAccountLimit(deps.state, env);
		case 'temp_ban_ip':
			return applyTempBanIp(deps.state, env);
		case 'limit_new_conversations':
			return applyLimitNewConversations(deps.state, env);
		case 'hide_profile':
			return applyHideProfile(deps.state, env);
		case 'hide_recent_messages':
			return applyHideRecentMessages(deps.state, env);
		case 'delete_user_messages':
			return applyDeleteUserMessages(deps.state, env);
		case 'pause_guild_invites':
			return applyPauseGuildInvites(deps.guildInvites, env);
		default:
			return Promise.resolve(outcomeOf(env as ActionEnvelope, 'unsupported'));
	}
}

async function publishOutcome(deps: AccountActionDeps, key: string, outcome: ActionOutcome): Promise<void> {
	if (deps.publishOutcome) {
		await deps.publishOutcome(key, outcome);
		return;
	}
	await emitActivity('action_outcome', key, outcome, workerMeta(), outcome.action_id);
}

async function handleActionMessage(deps: AccountActionDeps, msg: JsMsg): Promise<void> {
	const env = decodeActionEnvelope(msg.data);
	if (!env) {
		msg.term('undecodable action envelope');
		return;
	}
	let outcome: ActionOutcome;
	try {
		outcome = await applyAction(deps, env);
	} catch (error) {
		if (msg.info.deliveryCount < MAX_ACTION_ATTEMPTS) {
			Logger.warn({err: error, actionId: env.id, attempt: msg.info.deliveryCount}, 'Account action apply failed');
			msg.nak(Math.min(MAX_NAK_DELAY_MS, 1000 * msg.info.deliveryCount));
			return;
		}
		Logger.error({err: error, actionId: env.id}, 'Account action failed after all attempts');
		outcome = outcomeOf(env, 'failed', null, String(error).slice(0, 500));
	}
	await publishOutcome(deps, env.key, outcome);
	await msg.ackAck();
}

async function pause(state: ConsumerState, ms: number): Promise<void> {
	await sleep(ms, undefined, {signal: state.abort.signal, ref: false}).catch(() => undefined);
}

async function waitForActionsStream(deps: AccountActionDeps, state: ConsumerState): Promise<void> {
	let missingLogged = false;
	while (!state.stopped) {
		try {
			await deps.js.streams.get(ACTIONS_STREAM);
			if (missingLogged) Logger.info({stream: ACTIONS_STREAM}, 'Account action stream found, consuming actions');
			return;
		} catch (error) {
			if (!(error instanceof JetStreamApiError && error.code === JetStreamApiCodes.StreamNotFound)) {
				Logger.debug({err: error}, 'Account action stream check failed, retrying');
				await pause(state, deps.retryDelayMs ?? RETRY_DELAY_MS);
				continue;
			}
			if (!missingLogged) {
				Logger.info({stream: ACTIONS_STREAM}, 'Account action stream does not exist, checking again later');
				missingLogged = true;
			}
		}
		await pause(state, STREAM_RECHECK_MS);
	}
}

function actionsStreamReady(deps: AccountActionDeps, state: ConsumerState): Promise<void> {
	state.streamReady ??= waitForActionsStream(deps, state).finally(() => {
		state.streamReady = null;
	});
	return state.streamReady;
}

async function runActionPartition(deps: AccountActionDeps, partition: number, state: ConsumerState): Promise<void> {
	const retryDelayMs = deps.retryDelayMs ?? RETRY_DELAY_MS;
	while (!state.stopped) {
		await actionsStreamReady(deps, state);
		if (state.stopped) return;
		try {
			const consumer = await deps.js.consumers.get(ACTIONS_STREAM, effectsConsumer(partition));
			const info = await consumer.info(true);
			if (info.config.ack_policy !== AckPolicy.Explicit) {
				throw new Error(`Consumer ${effectsConsumer(partition)} must use explicit acks`);
			}
			const messages = await consumer.consume({max_messages: 1});
			state.loops.push(messages);
			if (state.stopped) {
				await messages.close();
				return;
			}
			for await (const msg of messages) {
				try {
					await handleActionMessage(deps, msg);
				} catch (error) {
					Logger.warn({err: error, partition}, 'Account action outcome could not be recorded');
					msg.nak(retryDelayMs);
				}
			}
		} catch (error) {
			if (state.stopped) return;
			Logger.debug({err: error, partition}, 'Account action consumer unavailable, retrying');
		}
		if (!state.stopped) await pause(state, retryDelayMs);
	}
}

export function startAccountActionConsumer(deps: AccountActionDeps): void {
	if (running) return;
	const state: ConsumerState = {
		stopped: false,
		abort: new AbortController(),
		loops: [],
		tasks: [],
		streamReady: null,
	};
	running = state;
	for (let p = 0; p < ACTION_PARTITIONS; p++) {
		state.tasks.push(runActionPartition(deps, p, state));
	}
}

export async function stopAccountActionConsumer(): Promise<void> {
	const state = running;
	if (!state) return;
	running = null;
	state.stopped = true;
	state.abort.abort();
	await Promise.allSettled(state.loops.map((messages) => messages.close()));
	await Promise.allSettled(state.tasks);
}
