// SPDX-License-Identifier: AGPL-3.0-or-later

import type {SoundType} from '@app/features/notification/utils/SoundUtils';

import Sound from '@app/features/ui/state/Sound';

interface SoundSettingsPatch {
	allSoundsDisabled?: boolean;
	soundType?: SoundType;
	enabled?: boolean;
}

export function playSound(sound: SoundType, loop?: boolean): void {
	Sound.playSound(sound, loop, undefined);
}

export function playSoundBypassingSelfDeafened(sound: SoundType, loop?: boolean): void {
	Sound.playSound(sound, loop, {bypassSelfDeafened: true});
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
	Sound.previewSound(sound);
}

export function stopAllSounds(): void {
	Sound.stopAllSounds();
}

export function updateSoundSettings(settings: SoundSettingsPatch): void {
	Sound.updateSettings(settings);
}

export function setMasterVolume(value: number): void {
	Sound.setMasterVolume(value);
}

export function setSoundOverride(soundType: SoundType, value: number): void {
	Sound.setSoundOverride(soundType, value);
}

export function clearSoundOverride(soundType: SoundType): void {
	Sound.clearSoundOverride(soundType);
}

export function clearAllSoundOverrides(): void {
	Sound.clearAllSoundOverrides();
}
