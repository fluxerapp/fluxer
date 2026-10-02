// SPDX-License-Identifier: AGPL-3.0-or-later

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
	applyLimitNewConversations,
	applyPhoneVerified,
	applySuspiciousFlags,
	applyTempBanIp,
	outcomeOf,
} from '@app/api/user/services/AccountStateApplier';
import {AckPolicy, type ConsumerMessages, type JetStreamClient, type JsMsg} from '@nats-io/jetstream';

export interface AccountActionDeps {
	js: Pick<JetStreamClient, 'consumers'>;
	state: AccountStateDeps;
	publishOutcome?: (key: string, outcome: ActionOutcome) => Promise<void>;
	now?: () => number;
	retryDelayMs?: number;
}

const RETRY_DELAY_MS = 5000;
const MAX_NAK_DELAY_MS = 30_000;

interface ConsumerState {
	stopped: boolean;
	loops: Array<ConsumerMessages>;
	tasks: Array<Promise<void>>;
}

let running: ConsumerState | null = null;

export function decodeActionEnvelope(data: Uint8Array | string): ActionEnvelope | null {
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
		case 'set_suspicious_flags':
			return applySuspiciousFlags(deps.state, env);
		case 'phone_verified':
			return applyPhoneVerified(deps.state, env);
		case 'temp_ban_ip':
			return applyTempBanIp(deps.state, env);
		case 'limit_new_conversations':
			return applyLimitNewConversations(deps.state, env);
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

export async function handleActionMessage(deps: AccountActionDeps, msg: JsMsg): Promise<void> {
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

async function runActionPartition(deps: AccountActionDeps, partition: number, state: ConsumerState): Promise<void> {
	const retryDelayMs = deps.retryDelayMs ?? RETRY_DELAY_MS;
	while (!state.stopped) {
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
		if (!state.stopped) await new Promise((resolve) => setTimeout(resolve, retryDelayMs).unref?.());
	}
}

export function startAccountActionConsumer(deps: AccountActionDeps): void {
	if (running) return;
	const state: ConsumerState = {stopped: false, loops: [], tasks: []};
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
	await Promise.allSettled(state.loops.map((messages) => messages.close()));
	await Promise.allSettled(state.tasks);
}
