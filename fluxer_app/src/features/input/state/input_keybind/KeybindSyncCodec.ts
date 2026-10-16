// SPDX-License-Identifier: AGPL-3.0-or-later

import type {CustomKeybindEntry, KeyCombo} from '@app/features/input/state/InputKeybind';
import {randomUuid} from '@app/features/platform/utils/RandomUuid';
import type {MessageInitShape} from '@bufbuild/protobuf';
import type {
	CustomKeybindSchema,
	KeybindComboSchema,
	KeybindSettingsSchema,
	CustomKeybind as SyncedCustomKeybind,
	KeybindCombo as SyncedKeybindCombo,
} from '@fluxer/schema/src/gen/fluxer/user/preferences/v1/preferences_pb';

type SyncedKeybindComboInit = MessageInitShape<typeof KeybindComboSchema>;
type SyncedCustomKeybindInit = MessageInitShape<typeof CustomKeybindSchema>;
type SyncedKeybindSettingsInit = MessageInitShape<typeof KeybindSettingsSchema>;

const TRANSMIT_MODES = ['voice_activity', 'voice_push_to_talk'] as const;

export type TransmitMode = (typeof TRANSMIT_MODES)[number];

export const DEFAULT_RELEASE_DELAY_MS = 20;
const MIN_RELEASE_DELAY_MS = 20;
const MAX_RELEASE_DELAY_MS = 2000;

export const clampReleaseDelay = (delayMs: number): number =>
	Math.max(MIN_RELEASE_DELAY_MS, Math.min(MAX_RELEASE_DELAY_MS, Math.round(delayMs)));

export const generateCustomKeybindId = (): string => randomUuid();

const toSyncedKeyCombo = (combo: KeyCombo): SyncedKeybindComboInit => ({
	key: combo.key ?? '',
	code: combo.code || undefined,
	ctrlOrMeta: !!combo.ctrlOrMeta,
	ctrl: !!combo.ctrl,
	alt: !!combo.alt,
	shift: !!combo.shift,
	meta: !!combo.meta,
	global: combo.global,
	enabled: combo.enabled,
	modifierOnly: !!combo.modifierOnly,
	bothSides: !!combo.bothSides,
	mouseButton: combo.mouseButton,
	gamepadButton: combo.gamepadButton,
});

const fromSyncedKeyCombo = (combo: SyncedKeybindCombo | undefined): KeyCombo => {
	if (!combo) return {key: '', enabled: true, global: true};
	return {
		key: combo.key ?? '',
		code: combo.code || undefined,
		ctrlOrMeta: combo.ctrlOrMeta || undefined,
		ctrl: combo.ctrl || undefined,
		alt: combo.alt || undefined,
		shift: combo.shift || undefined,
		meta: combo.meta || undefined,
		global: combo.global,
		enabled: combo.enabled,
		modifierOnly: combo.modifierOnly || undefined,
		bothSides: combo.bothSides || undefined,
		mouseButton: combo.mouseButton,
		gamepadButton: combo.gamepadButton,
	};
};

export const toSyncedCustomKeybind = (entry: CustomKeybindEntry): SyncedCustomKeybindInit => ({
	id: entry.id,
	action: entry.action ?? undefined,
	combo: toSyncedKeyCombo(entry.combo),
	enabled: entry.enabled,
});

export const fromSyncedCustomKeybind = (entry: SyncedCustomKeybind): CustomKeybindEntry => ({
	id: entry.id || generateCustomKeybindId(),
	action: entry.action || null,
	combo: fromSyncedKeyCombo(entry.combo),
	enabled: entry.enabled,
});

export const normalizeTransmitMode = (mode: string | undefined): TransmitMode => {
	if (mode && TRANSMIT_MODES.includes(mode as TransmitMode)) {
		return mode as TransmitMode;
	}
	return 'voice_activity';
};

export const toSyncedKeybindSettings = (store: {
	customKeybinds: Array<CustomKeybindEntry>;
	transmitMode: TransmitMode;
	pushToTalkReleaseDelay: number;
}): SyncedKeybindSettingsInit => ({
	customKeybinds: store.customKeybinds.map(toSyncedCustomKeybind),
	transmitMode: store.transmitMode,
	pushToTalkReleaseDelayMs:
		store.pushToTalkReleaseDelay === DEFAULT_RELEASE_DELAY_MS ? undefined : store.pushToTalkReleaseDelay,
});
