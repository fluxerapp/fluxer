// SPDX-License-Identifier: AGPL-3.0-or-later

import {CUSTOM_SOUND_MAX_SIZE_BYTES} from '@app/features/app/config/I18nDisplayConstants';
import {TRY_AGAIN_IN_A_MOMENT_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {openFilePicker} from '@app/features/messaging/utils/FilePickerUtils';
import {formatFileSize} from '@app/features/messaging/utils/FileUtils';
import * as CustomSoundDB from '@app/features/notification/utils/CustomSoundDB';
import {getSoundLabels} from '@app/features/notification/utils/SoundLabels';
import {clearCustomSoundCache, type SoundType} from '@app/features/notification/utils/SoundUtils';
import * as SoundCommands from '@app/features/ui/commands/SoundCommands';
import * as ToastCommands from '@app/features/ui/commands/ToastCommands';
import {showUserErrorModal} from '@app/features/user/utils/UserErrorModalUtils';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {useCallback, useEffect, useMemo, useState} from 'react';

const INVALID_AUDIO_FILE_DESCRIPTOR = msg({
	message: 'Invalid audio file',
	comment: 'Error message in the use sound settings.',
});
const CHOOSE_A_SUPPORTED_AUDIO_FILE_DESCRIPTOR = msg({
	message: 'Choose a supported audio file and try again.',
	comment: 'Body of the error modal shown when a custom notification sound file is invalid.',
});
const FILE_IS_TOO_LARGE_MAXIMUM_SIZE_IS_DESCRIPTOR = msg({
	message: 'File is too large. Maximum size is {maxSize}.',
	comment: 'Form validation error for an asset that exceeds the maximum file size.',
});
const CUSTOM_SOUND_UPLOADED_SUCCESSFULLY_DESCRIPTOR = msg({
	message: 'Custom sound uploaded successfully',
	comment: 'Label in the use sound settings.',
});
const FAILED_TO_UPLOAD_CUSTOM_SOUND_DESCRIPTOR = msg({
	message: 'Failed to upload custom sound',
	comment: 'Error message in the use sound settings.',
});
const CUSTOM_SOUND_REMOVED_DESCRIPTOR = msg({
	message: 'Custom sound removed',
	comment: 'Short label in the use sound settings. Keep it concise. Keep the tone plain and specific.',
});
const FAILED_TO_REMOVE_CUSTOM_SOUND_DESCRIPTOR = msg({
	message: 'Failed to remove custom sound',
	comment: 'Error message in the use sound settings. Keep the tone plain and specific.',
});

export function useSoundSettings() {
	const {i18n} = useLingui();
	const soundTypeLabels = useMemo(() => getSoundLabels(i18n), [i18n.locale]);
	const [customSounds, setCustomSounds] = useState<Record<SoundType, CustomSoundDB.CustomSound | null>>(
		Object.keys(soundTypeLabels).reduce(
			(acc, soundType) => {
				acc[soundType as SoundType] = null;
				return acc;
			},
			{} as Record<SoundType, CustomSoundDB.CustomSound | null>,
		),
	);
	useEffect(() => {
		const loadCustomSounds = async () => {
			try {
				const allSounds = await CustomSoundDB.getAllCustomSounds();
				const soundsMap: Record<string, CustomSoundDB.CustomSound | null> = {};
				Object.keys(soundTypeLabels).forEach((soundType) => {
					soundsMap[soundType] = allSounds.find((s) => s.soundType === soundType) || null;
				});
				setCustomSounds(soundsMap as Record<SoundType, CustomSoundDB.CustomSound | null>);
			} catch {}
		};
		loadCustomSounds();
	}, [soundTypeLabels]);
	const handleToggleAllSounds = (value: boolean) => {
		SoundCommands.updateSoundSettings({allSoundsDisabled: value});
	};
	const handleToggleSound = (soundType: SoundType, enabled: boolean) => {
		SoundCommands.updateSoundSettings({soundType, enabled});
	};
	const handleEnableAllSounds = () => {
		Object.keys(soundTypeLabels).forEach((soundType) => {
			SoundCommands.updateSoundSettings({
				soundType: soundType as SoundType,
				enabled: true,
			});
		});
	};
	const handleDisableAllSounds = () => {
		Object.keys(soundTypeLabels).forEach((soundType) => {
			SoundCommands.updateSoundSettings({
				soundType: soundType as SoundType,
				enabled: false,
			});
		});
	};
	const handlePreviewSound = useCallback((soundType: SoundType) => {
		SoundCommands.stopAllSounds();
		SoundCommands.previewSound(soundType);
	}, []);
	const handleMasterVolumeChange = useCallback((value: number) => {
		SoundCommands.setMasterVolume(value);
	}, []);
	const handleSoundOverrideChange = useCallback((soundType: SoundType, value: number) => {
		SoundCommands.setSoundOverride(soundType, value);
	}, []);
	const handleSoundOverrideReset = useCallback((soundType: SoundType) => {
		SoundCommands.clearSoundOverride(soundType);
	}, []);
	const handleAllOverridesReset = useCallback(() => {
		SoundCommands.clearAllSoundOverrides();
	}, []);
	useEffect(() => {
		return () => {
			SoundCommands.stopAllSounds();
		};
	}, []);
	const handleCustomSoundUpload = useCallback(
		async (soundType: SoundType, file: File | null) => {
			if (!file) {
				return;
			}
			const validation = CustomSoundDB.isValidAudioFile(file);
			if (!validation.valid) {
				showUserErrorModal(
					i18n._(INVALID_AUDIO_FILE_DESCRIPTOR),
					validation.reason === 'too_large'
						? i18n._(FILE_IS_TOO_LARGE_MAXIMUM_SIZE_IS_DESCRIPTOR, {
								maxSize: formatFileSize(i18n.locale, CUSTOM_SOUND_MAX_SIZE_BYTES),
							})
						: i18n._(CHOOSE_A_SUPPORTED_AUDIO_FILE_DESCRIPTOR),
				);
				return;
			}
			try {
				await CustomSoundDB.saveCustomSound(soundType, file, file.name);
				clearCustomSoundCache(soundType);
				const customSound = await CustomSoundDB.getCustomSound(soundType);
				setCustomSounds((prev) => ({
					...prev,
					[soundType]: customSound,
				}));
				ToastCommands.createToast({
					type: 'success',
					children: i18n._(CUSTOM_SOUND_UPLOADED_SUCCESSFULLY_DESCRIPTOR),
				});
			} catch {
				showUserErrorModal(i18n._(FAILED_TO_UPLOAD_CUSTOM_SOUND_DESCRIPTOR), i18n._(TRY_AGAIN_IN_A_MOMENT_DESCRIPTOR));
			}
		},
		[i18n],
	);
	const handleCustomSoundDelete = useCallback(
		async (soundType: SoundType) => {
			try {
				await CustomSoundDB.deleteCustomSound(soundType);
				clearCustomSoundCache(soundType);
				setCustomSounds((prev) => ({
					...prev,
					[soundType]: null,
				}));
				ToastCommands.createToast({
					type: 'success',
					children: i18n._(CUSTOM_SOUND_REMOVED_DESCRIPTOR),
				});
			} catch {
				showUserErrorModal(i18n._(FAILED_TO_REMOVE_CUSTOM_SOUND_DESCRIPTOR), i18n._(TRY_AGAIN_IN_A_MOMENT_DESCRIPTOR));
			}
		},
		[i18n],
	);
	const handleUploadClick = useCallback(
		async (soundType: SoundType) => {
			const [file] = await openFilePicker({accept: CustomSoundDB.SUPPORTED_MIME_TYPES.join(',')});
			await handleCustomSoundUpload(soundType, file ?? null);
		},
		[handleCustomSoundUpload],
	);
	return {
		soundTypeLabels,
		customSounds,
		handleToggleAllSounds,
		handleToggleSound,
		handleEnableAllSounds,
		handleDisableAllSounds,
		handlePreviewSound,
		handleUploadClick,
		handleCustomSoundDelete,
		handleMasterVolumeChange,
		handleSoundOverrideChange,
		handleSoundOverrideReset,
		handleAllOverridesReset,
	};
}
