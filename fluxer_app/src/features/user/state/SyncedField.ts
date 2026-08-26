// SPDX-License-Identifier: AGPL-3.0-or-later

import {Logger} from '@app/features/platform/utils/AppLogger';
import {verifyRoundtripStability} from '@app/features/user/state/SyncedFieldRoundtrip';
import {
	createSyncedFieldMachineSnapshot,
	type SyncedFieldCommand,
	type SyncedFieldFailureReason,
	type SyncedFieldMachineEvent,
	selectSyncedFieldMachineModel,
	transitionSyncedFieldMachineSnapshot,
} from '@app/features/user/state/SyncedFieldStateMachine';
import type {SyncedPreferences, SyncedPreferencesField} from '@app/features/user/state/SyncedPreferencesEngine';
import {create, equals, type Message, type MessageInitShape, toBinary} from '@bufbuild/protobuf';
import type {GenMessage} from '@bufbuild/protobuf/codegenv2';
import {comparer, reaction, runInAction} from 'mobx';

export {verifyRoundtripStability};

const logger = new Logger('SyncedField');

function isMessageSchema(value: unknown): value is GenMessage<Message> {
	return (
		value != null &&
		typeof value === 'object' &&
		typeof (value as {typeName?: unknown}).typeName === 'string' &&
		Array.isArray((value as {fields?: unknown}).fields)
	);
}

export interface SyncedFieldConfig<
	T extends object,
	F extends SyncedPreferencesField,
	M extends Message & NonNullable<SyncedPreferences[F]>,
> {
	readonly field: F;
	readonly schema: GenMessage<M>;
	readonly persist: ReadonlyArray<keyof T>;
	toMessage(store: T): MessageInitShape<GenMessage<M>>;
	applyMessage(store: T, message: M): void;
	readonly version?: number;
	readonly debounceMs?: number;
	readonly syncAcrossTabs?: boolean;
	readonly maxEncodedBytes?: number;
	enabled?(): boolean;
	mergeRemote?(local: M, incoming: M): MessageInitShape<GenMessage<M>>;
}

export async function makeSyncedField<
	T extends object,
	F extends SyncedPreferencesField,
	M extends Message & NonNullable<SyncedPreferences[F]>,
>(store: T, config: SyncedFieldConfig<T, F, M>): Promise<void> {
	const tag = `${store.constructor.name}.${String(config.field)}`;
	const schema = config.schema;
	if (!isMessageSchema(schema)) {
		logger.error(`${tag}: invalid protobuf schema; synced persistence disabled.`);
		return;
	}
	const isEnabled = (): boolean => config.enabled?.() ?? true;
	let machine = createSyncedFieldMachineSnapshot();
	let ownerUserId: string | null = null;
	let observedUserId: string | null = null;
	const suspend = (reason: SyncedFieldFailureReason, message: string, error?: unknown): void => {
		logger.error(`${tag}: ${message}; synced persistence disabled:`, error);
		transitionMachine({
			type: 'sync.failed',
			failure: {
				reason,
				message,
				error,
			},
		});
	};
	const buildMessage = (): M => create(schema, config.toMessage(store)) as NonNullable<SyncedPreferences[F]> as M;
	const buildMessageOrSuspend = (message: string): M | null => {
		if (!selectSyncedFieldMachineModel(machine).isActive) return null;
		try {
			return buildMessage();
		} catch (error) {
			suspend('build-local', message, error);
			return null;
		}
	};
	const getRemoteSnapshot = (): SyncedPreferences[F] | undefined => {
		if (!selectSyncedFieldMachineModel(machine).isActive) return undefined;
		if (!isEnabled()) return undefined;
		try {
			return UserSettings.getSubPreference(config.field);
		} catch (error) {
			suspend('read-remote', 'failed to read the remote snapshot', error);
			return undefined;
		}
	};
	const compareRemote = (snapshot: M): void => {
		const candidate = buildMessageOrSuspend('failed to build the local snapshot before applying a remote update');
		if (candidate == null) return;
		try {
			transitionOnly({type: equals(schema, snapshot, candidate) ? 'sync.remoteMatched' : 'sync.remoteNeedsApply'});
		} catch (error) {
			suspend('compare-remote', 'failed to compare the remote snapshot with local state', error);
		}
	};
	const applyRemote = (snapshot: M): void => {
		let effective = snapshot;
		if (config.mergeRemote && (ownerUserId === null || ownerUserId === observedUserId)) {
			try {
				effective = create(schema, config.mergeRemote(buildMessage(), snapshot)) as M;
			} catch (error) {
				logger.error(`${tag}: mergeRemote threw; applying the remote snapshot as-is:`, error);
				effective = snapshot;
			}
		}
		try {
			runInAction(() => {
				config.applyMessage(store, effective);
			});
		} catch (error) {
			logger.error(`${tag}: applyMessage threw:`, error);
		}
		ownerUserId = observedUserId;
		transitionOnly({type: 'sync.applyFinished'});
	};
	const preparePush = (candidate: M): void => {
		ownerUserId ??= observedUserId;
		try {
			const current = UserSettings.getSubPreference(config.field);
			if (current !== undefined && equals(schema, current as M, candidate)) {
				transitionOnly({type: 'sync.localAlreadySynced'});
				return;
			}
			const encodedBytes = toBinary(schema, candidate).length;
			if (encodedBytes > maxEncodedBytes) {
				logger.error(
					`${tag}: payload of ${encodedBytes} bytes exceeds per-field budget of ${maxEncodedBytes}; push dropped.`,
				);
				transitionOnly({type: 'sync.localAlreadySynced'});
				return;
			}
			const stability = verifyRoundtripStability<T, M>({
				schema,
				store,
				toMessage: config.toMessage,
				applyMessage: config.applyMessage,
				candidate,
			});
			if (stability.threw !== undefined) {
				suspend('roundtrip-threw', 'applyMessage threw during roundtrip probe', stability.threw);
				return;
			}
			if (!stability.stable) {
				suspend(
					'roundtrip-unstable',
					'roundtrip-stability check failed (toMessage → applyMessage → toMessage produced a different message)',
				);
				return;
			}
		} catch (error) {
			suspend('encode-local', 'failed to encode or validate the local snapshot', error);
			return;
		}
		transitionOnly({type: 'sync.localReadyToPush'});
	};
	const push = (candidate: M): void => {
		void UserSettings.setSubPreference(config.field, candidate).catch((error) => {
			const status =
				error != null && typeof error === 'object' && typeof (error as {status?: unknown}).status === 'number'
					? ((error as {status: number}).status as number)
					: null;
			if (status === 429) return;
			logger.warn(`${tag}: failed to sync to server:`, error);
		});
		transitionOnly({type: 'sync.commandHandled'});
	};
	const executeCommand = (command: SyncedFieldCommand): void => {
		switch (command.type) {
			case 'compareRemote':
				compareRemote(command.snapshot as M);
				return;
			case 'applyRemote':
				applyRemote(command.snapshot as M);
				return;
			case 'preparePush':
				preparePush(command.candidate as M);
				return;
			case 'pushLocal':
				push(command.candidate as M);
				return;
		}
	};
	const processCommands = (): void => {
		for (let i = 0; i < 8; i++) {
			const command = selectSyncedFieldMachineModel(machine).command;
			if (command == null) return;
			executeCommand(command);
		}
		suspend('machine-loop', 'state machine command loop exceeded the safety limit');
	};
	function transitionOnly(event: SyncedFieldMachineEvent): void {
		machine = transitionSyncedFieldMachineSnapshot(machine, event);
	}
	function transitionMachine(event: SyncedFieldMachineEvent): void {
		transitionOnly(event);
		processCommands();
	}
	await Promise.resolve();
	const maxEncodedBytes = config.maxEncodedBytes ?? 65_536;
	const UserSettings = (await import('@app/features/user/state/UserSettings')).default;
	const SessionManager = (await import('@app/features/platform/state/AuthSession')).default;
	const applyDefaultsForUserChange = (userId: string): void => {
		try {
			runInAction(() => {
				config.applyMessage(store, create(schema) as M);
			});
		} catch (error) {
			logger.error(`${tag}: applyMessage threw while resetting to defaults after an account change:`, error);
		}
		ownerUserId = userId;
	};
	try {
		reaction(
			() => ({
				snapshot: getRemoteSnapshot(),
				userId: SessionManager?.userId ?? null,
				hydrated: UserSettings.isHydrated(),
			}),
			({snapshot, userId, hydrated}) => {
				observedUserId = userId;
				if (
					snapshot === undefined &&
					hydrated &&
					userId != null &&
					ownerUserId != null &&
					ownerUserId !== userId &&
					selectSyncedFieldMachineModel(machine).isActive
				) {
					applyDefaultsForUserChange(userId);
				}
				transitionMachine({
					type: 'sync.remoteObserved',
					enabled: isEnabled(),
					snapshot,
				});
			},
			{fireImmediately: true, equals: comparer.shallow},
		);
		reaction(
			() => (isEnabled() ? buildMessageOrSuspend('failed to build the local snapshot for observation') : null),
			(candidate) => {
				transitionMachine({
					type: 'sync.localObserved',
					enabled: isEnabled(),
					candidate,
				});
			},
			config.debounceMs != null && config.debounceMs > 0 ? {delay: config.debounceMs} : undefined,
		);
	} catch (error) {
		suspend('initialize-reactions', 'failed to initialize reactions', error);
	}
}
