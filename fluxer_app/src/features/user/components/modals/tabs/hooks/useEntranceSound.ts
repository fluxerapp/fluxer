// SPDX-License-Identifier: AGPL-3.0-or-later

import {LimitResolver} from '@app/features/app/utils/LimitResolverAdapter';
import {isLimitToggleEnabled} from '@app/features/app/utils/LimitUtils';
import {TRY_AGAIN_IN_A_MOMENT_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {openFilePicker} from '@app/features/messaging/utils/FilePickerUtils';
import {formatFileSize} from '@app/features/messaging/utils/FileUtils';
import {
	ENTRANCE_SOUND_FILE_PICKER_ACCEPT,
	type EntranceSoundFileValidationResult,
	isValidEntranceSoundFile,
} from '@app/features/notification/utils/EntranceSoundClientValidators';
import type {EntranceSoundScope} from '@app/features/notification/utils/EntranceSoundScopes';
import {Logger} from '@app/features/platform/utils/AppLogger';
import * as PremiumModalCommands from '@app/features/premium/commands/PremiumModalCommands';
import {shouldShowPremiumFeatures} from '@app/features/premium/utils/PremiumUtils';
import * as ToastCommands from '@app/features/ui/commands/ToastCommands';
import {showUserErrorModal} from '@app/features/user/utils/UserErrorModalUtils';
import {
	openEntranceSoundTrimmerModal,
	type TrimmedAudioResult,
} from '@app/features/voice/components/EntranceSoundTrimmerModal';
import EntranceSoundPlaybackEngine from '@app/features/voice/engine/EntranceSoundPlaybackEngine';
import EntranceSoundLibrary, {type EntranceSoundEntry} from '@app/features/voice/state/EntranceSoundLibrary';
import {ENTRANCE_SOUND_MAX_BYTES, ENTRANCE_SOUND_MAX_PER_USER} from '@fluxer/constants/src/EntranceSoundConstants';
import type {I18n} from '@lingui/core';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {useCallback, useEffect, useMemo, useState} from 'react';

const INVALID_AUDIO_FILE_DESCRIPTOR = msg({
	message: 'Invalid audio file',
	comment: 'Error message in the use entrance sound.',
});
const ENTRANCE_SOUND_TOO_LARGE_DESCRIPTOR = msg({
	message: 'Entrance sound files must be {limit} or less.',
	comment: 'Error modal body shown when an entrance sound upload exceeds the client-side size limit.',
});
const ENTRANCE_SOUND_INVALID_FORMAT_DESCRIPTOR = msg({
	message: 'Unsupported audio file.',
	comment: 'Error modal body shown when an entrance sound upload has an unsupported audio format.',
});
const ENTRANCE_SOUND_SAVED_DESCRIPTOR = msg({
	message: 'Entrance sound saved',
	comment: 'Short label in the use entrance sound. Keep it concise.',
});
const COULDN_T_SAVE_ENTRANCE_SOUND_DESCRIPTOR = msg({
	message: "Couldn't save entrance sound",
	comment: 'Error message in the use entrance sound.',
});
const ENTRANCE_SOUND_REMOVED_DESCRIPTOR = msg({
	message: 'Entrance sound removed',
	comment: 'Short label in the use entrance sound. Keep it concise. Keep the tone plain and specific.',
});
const COULDN_T_REMOVE_ENTRANCE_SOUND_DESCRIPTOR = msg({
	message: "Couldn't remove entrance sound",
	comment: 'Error message in the use entrance sound. Keep the tone plain and specific.',
});
const LIBRARY_FULL_TITLE_DESCRIPTOR = msg({
	message: 'Entrance sound library full',
	comment: 'Title of the error modal shown when the entrance sound library quota is reached.',
});
const LIBRARY_FULL_DESCRIPTOR = msg({
	message: 'Library full ({max} sounds max). Delete one before adding another.',
	comment: 'Error modal body shown when entrance sound library quota is reached.',
});
const ENTRANCE_SOUNDS_NOT_ENABLED_TITLE_DESCRIPTOR = msg({
	message: 'Entrance sounds unavailable',
	comment: 'Title of the error modal shown when custom entrance sounds are disabled on the instance.',
});
const ENTRANCE_SOUNDS_NOT_ENABLED_DESCRIPTOR = msg({
	message: 'Custom entrance sounds are not enabled on this instance.',
	comment: 'Error modal body shown when custom entrance sounds are disabled on the instance.',
});

const logger = new Logger('useEntranceSound');

export interface UseEntranceSoundReturn {
	library: Array<EntranceSoundEntry>;
	libraryFull: boolean;
	libraryLoaded: boolean;
	selectedSound: EntranceSoundEntry | null;
	resolvedSound: EntranceSoundEntry | null;
	resolvedScope: EntranceSoundScope | null;
	inheritedFromScope: EntranceSoundScope | null;
	isPreviewing: boolean;
	setSoundForScope: (soundId: string | null) => Promise<void>;
	openUploadDialog: () => Promise<void>;
	renameSound: (soundId: string, name: string) => Promise<void>;
	deleteSound: (soundId: string) => Promise<void>;
	previewSound: (soundId: string) => Promise<void>;
}

async function blobToBase64(blob: Blob): Promise<string> {
	const buffer = await blob.arrayBuffer();
	const bytes = new Uint8Array(buffer);
	let binary = '';
	const chunkSize = 0x8000;
	for (let i = 0; i < bytes.length; i += chunkSize) {
		const chunk = bytes.subarray(i, i + chunkSize);
		binary += String.fromCharCode(...chunk);
	}
	return typeof btoa === 'function' ? btoa(binary) : Buffer.from(binary, 'binary').toString('base64');
}

function deriveLibraryName(file: File): string {
	const trimmed = file.name.trim();
	if (!trimmed) return 'Entrance sound';
	const lastDot = trimmed.lastIndexOf('.');
	const base = lastDot > 0 ? trimmed.slice(0, lastDot) : trimmed;
	return base.length === 0 ? 'Entrance sound' : base.slice(0, 32);
}

function validationErrorMessage(
	i18n: I18n,
	validation: Extract<EntranceSoundFileValidationResult, {valid: false}>,
): string {
	if (validation.reason === 'too_large') {
		return i18n._(ENTRANCE_SOUND_TOO_LARGE_DESCRIPTOR, {limit: formatFileSize(i18n.locale, ENTRANCE_SOUND_MAX_BYTES)});
	}
	return i18n._(ENTRANCE_SOUND_INVALID_FORMAT_DESCRIPTOR);
}

function useEntranceSoundImpl(selectedScope: EntranceSoundScope): UseEntranceSoundReturn {
	const {i18n} = useLingui();
	const [isPreviewing, setIsPreviewing] = useState(false);
	const hasEntranceSounds = useMemo(
		() =>
			isLimitToggleEnabled(
				{feature_voice_entrance_sounds: LimitResolver.resolve({key: 'feature_voice_entrance_sounds', fallback: 0})},
				'feature_voice_entrance_sounds',
			),
		[],
	);
	useEffect(() => {
		void EntranceSoundLibrary.load();
	}, []);
	const library = EntranceSoundLibrary.list;
	const selectedSoundId = EntranceSoundLibrary.getSelection(selectedScope);
	const selectedSound = EntranceSoundLibrary.getById(selectedSoundId);
	const resolved = EntranceSoundLibrary.resolveForScope(selectedScope);
	const resolvedSound = resolved?.sound ?? null;
	const resolvedScope = resolved?.scope ?? null;
	const inheritedFromScope = selectedSound ? null : resolvedScope;
	const libraryFull = library.length >= ENTRANCE_SOUND_MAX_PER_USER;

	const setSoundForScope = useCallback(
		async (soundId: string | null) => {
			try {
				await EntranceSoundLibrary.setSelection(selectedScope, soundId);
				ToastCommands.createToast({
					type: 'success',
					children: i18n._(soundId === null ? ENTRANCE_SOUND_REMOVED_DESCRIPTOR : ENTRANCE_SOUND_SAVED_DESCRIPTOR),
				});
			} catch (error) {
				logger.error('Failed to set entrance sound selection', error);
				showUserErrorModal(
					i18n._(
						soundId === null ? COULDN_T_REMOVE_ENTRANCE_SOUND_DESCRIPTOR : COULDN_T_SAVE_ENTRANCE_SOUND_DESCRIPTOR,
					),
					i18n._(TRY_AGAIN_IN_A_MOMENT_DESCRIPTOR),
				);
			}
		},
		[selectedScope, i18n],
	);

	const uploadTrimmedResult = useCallback(
		async (sourceName: string, trimmed: TrimmedAudioResult) => {
			try {
				const base64 = await blobToBase64(trimmed.blob);
				const entry = await EntranceSoundLibrary.uploadSound({name: sourceName, base64Audio: base64});
				await EntranceSoundLibrary.setSelection(selectedScope, entry.id);
				ToastCommands.createToast({type: 'success', children: i18n._(ENTRANCE_SOUND_SAVED_DESCRIPTOR)});
			} catch (error) {
				logger.error('Failed to upload trimmed entrance sound', error);
				showUserErrorModal(i18n._(COULDN_T_SAVE_ENTRANCE_SOUND_DESCRIPTOR), i18n._(TRY_AGAIN_IN_A_MOMENT_DESCRIPTOR));
				throw error;
			}
		},
		[selectedScope, i18n],
	);

	const openUploadDialog = useCallback(async () => {
		if (!hasEntranceSounds) {
			if (shouldShowPremiumFeatures()) {
				PremiumModalCommands.open();
			} else {
				showUserErrorModal(
					i18n._(ENTRANCE_SOUNDS_NOT_ENABLED_TITLE_DESCRIPTOR),
					i18n._(ENTRANCE_SOUNDS_NOT_ENABLED_DESCRIPTOR),
				);
			}
			return;
		}
		if (libraryFull) {
			showUserErrorModal(
				i18n._(LIBRARY_FULL_TITLE_DESCRIPTOR),
				i18n._(LIBRARY_FULL_DESCRIPTOR, {max: ENTRANCE_SOUND_MAX_PER_USER}),
			);
			return;
		}
		const [file] = await openFilePicker({accept: ENTRANCE_SOUND_FILE_PICKER_ACCEPT});
		if (!file) return;
		const validation = isValidEntranceSoundFile(file);
		if (!validation.valid) {
			showUserErrorModal(i18n._(INVALID_AUDIO_FILE_DESCRIPTOR), validationErrorMessage(i18n, validation));
			return;
		}
		const proposedName = deriveLibraryName(file);
		openEntranceSoundTrimmerModal({
			sourceFile: file,
			onConfirm: (trimmed) => uploadTrimmedResult(proposedName, trimmed),
		});
	}, [hasEntranceSounds, libraryFull, i18n, uploadTrimmedResult]);

	const renameSound = useCallback(
		async (soundId: string, name: string) => {
			try {
				await EntranceSoundLibrary.renameSound(soundId, name);
			} catch (error) {
				logger.error('Failed to rename entrance sound', error);
				showUserErrorModal(i18n._(COULDN_T_SAVE_ENTRANCE_SOUND_DESCRIPTOR), i18n._(TRY_AGAIN_IN_A_MOMENT_DESCRIPTOR));
			}
		},
		[i18n],
	);

	const deleteSound = useCallback(
		async (soundId: string) => {
			try {
				await EntranceSoundLibrary.deleteSound(soundId);
				ToastCommands.createToast({type: 'success', children: i18n._(ENTRANCE_SOUND_REMOVED_DESCRIPTOR)});
			} catch (error) {
				logger.error('Failed to delete entrance sound', error);
				showUserErrorModal(i18n._(COULDN_T_REMOVE_ENTRANCE_SOUND_DESCRIPTOR), i18n._(TRY_AGAIN_IN_A_MOMENT_DESCRIPTOR));
			}
		},
		[i18n],
	);

	const previewSound = useCallback(
		async (soundId: string) => {
			const sound = EntranceSoundLibrary.getById(soundId);
			if (!sound || isPreviewing) return;
			try {
				setIsPreviewing(true);
				const buffer = await EntranceSoundPlaybackEngine.fetchBuffer(sound.url, sound.hash);
				if (buffer) {
					EntranceSoundPlaybackEngine.playPreview(buffer);
					setTimeout(() => setIsPreviewing(false), sound.durationMs + 50);
				} else {
					setIsPreviewing(false);
				}
			} catch (error) {
				logger.error('Failed to preview entrance sound', error);
				setIsPreviewing(false);
			}
		},
		[isPreviewing],
	);

	return {
		library,
		libraryFull,
		libraryLoaded: EntranceSoundLibrary.loaded,
		selectedSound,
		resolvedSound,
		resolvedScope,
		inheritedFromScope,
		isPreviewing,
		setSoundForScope,
		openUploadDialog,
		renameSound,
		deleteSound,
		previewSound,
	};
}

export const useEntranceSound = useEntranceSoundImpl;
