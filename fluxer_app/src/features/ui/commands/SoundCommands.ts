// SPDX-License-Identifier: AGPL-3.0-or-later

import type {SoundType} from '@app/features/notification/utils/SoundUtils';
import type {SoundPlayOptions} from '@app/features/ui/state/Sound';
import Sound from '@app/features/ui/state/Sound';

interface SoundSettingsPatch {
	allSoundsDisabled?: boolean;
	soundType?: SoundType;
	enabled?: boolean;
}

type SoundIntent =
	| {kind: 'play'; sound: SoundType; loop?: boolean; options?: SoundPlayOptions}
	| {kind: 'preview'; sound: SoundType}
	| {kind: 'stop-all'}
	| {kind: 'settings'; settings: SoundSettingsPatch}
	| {kind: 'master-volume'; value: number}
	| {kind: 'override'; soundType: SoundType; value: number}
	| {kind: 'clear-override'; soundType: SoundType}
	| {kind: 'clear-all-overrides'};

function dispatchSoundIntent(intent: SoundIntent): void {
	switch (intent.kind) {
		case 'play':
			Sound.playSound(intent.sound, intent.loop, intent.options);
			return;
		case 'preview':
			Sound.previewSound(intent.sound);
			return;
		case 'stop-all':
			Sound.stopAllSounds();
			return;
		case 'settings':
			Sound.updateSettings(intent.settings);
			return;
		case 'master-volume':
			Sound.setMasterVolume(intent.value);
			return;
		case 'override':
			Sound.setSoundOverride(intent.soundType, intent.value);
			return;
		case 'clear-override':
			Sound.clearSoundOverride(intent.soundType);
			return;
		case 'clear-all-overrides':
			Sound.clearAllSoundOverrides();
			return;
	}
}

export function playSound(sound: SoundType, loop?: boolean): void {
	dispatchSoundIntent({kind: 'play', sound, loop});
}

export function playSoundBypassingSelfDeafened(sound: SoundType, loop?: boolean): void {
	dispatchSoundIntent({kind: 'play', sound, loop, options: {bypassSelfDeafened: true}});
}

export function playOneShotSoundImmediatelyBypassingSelfDeafened(
	sound: SoundType,
	signal?: AbortSignal,
	options: {ignoreGroupCooldown?: boolean} = {},
): Promise<boolean> {
	return Sound.playOneShotSoundImmediately(
		sound,
		{bypassSelfDeafened: true, ignoreGroupCooldown: options.ignoreGroupCooldown === true},
		signal,
	);
}

export function previewSound(sound: SoundType): void {
	dispatchSoundIntent({kind: 'preview', sound});
}

export function stopAllSounds(): void {
	dispatchSoundIntent({kind: 'stop-all'});
}

export function updateSoundSettings(settings: SoundSettingsPatch): void {
	dispatchSoundIntent({kind: 'settings', settings});
}

export function setMasterVolume(value: number): void {
	dispatchSoundIntent({kind: 'master-volume', value});
}

export function setSoundOverride(soundType: SoundType, value: number): void {
	dispatchSoundIntent({kind: 'override', soundType, value});
}

export function clearSoundOverride(soundType: SoundType): void {
	dispatchSoundIntent({kind: 'clear-override', soundType});
}

export function clearAllSoundOverrides(): void {
	dispatchSoundIntent({kind: 'clear-all-overrides'});
}
