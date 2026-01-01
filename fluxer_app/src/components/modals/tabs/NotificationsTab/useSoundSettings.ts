/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Fluxer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fluxer. If not, see <https://www.gnu.org/licenses/>.
 */

import {useLingui} from '@lingui/react/macro';
import React from 'react';
import * as PremiumModalActionCreators from '~/actions/PremiumModalActionCreators';
import * as SoundActionCreators from '~/actions/SoundActionCreators';
import * as ToastActionCreators from '~/actions/ToastActionCreators';
import UserStore from '~/stores/UserStore';
import * as CustomSoundDB from '~/utils/CustomSoundDB';
import {openFilePicker} from '~/utils/FilePickerUtils';
import {getSoundLabels} from '~/utils/SoundLabels';
import {clearCustomSoundCache, type SoundType} from '~/utils/SoundUtils';

export const useSoundSettings = () => {
	const {t, i18n} = useLingui();
	const user = UserStore.currentUser;
	const hasPremium = React.useMemo(() => user?.isPremium() ?? false, [user]);
	const [customSounds, setCustomSounds] = React.useState<Record<SoundType, CustomSoundDB.CustomSound | null>>(
		{} as any,
	);

	const soundTypeLabels = React.useMemo(() => getSoundLabels(i18n), [i18n]);

	React.useEffect(() => {
		const loadCustomSounds = async () => {
			try {
				const allSounds = await CustomSoundDB.getAllCustomSounds();
				const soundsMap: Record<string, CustomSoundDB.CustomSound | null> = {};
				Object.keys(soundTypeLabels).forEach((soundType) => {
					soundsMap[soundType] = allSounds.find((s) => s.soundType === soundType) || null;
				});
				setCustomSounds(soundsMap as any);
			} catch {}
		};
		loadCustomSounds();
	}, [soundTypeLabels]);

	const handleToggleAllSounds = (value: boolean) => {
		SoundActionCreators.updateSoundSettings({allSoundsDisabled: value});
	};

	const handleToggleSound = (soundType: SoundType, enabled: boolean) => {
		SoundActionCreators.updateSoundSettings({soundType, enabled});
	};

	const handleEnableAllSounds = () => {
		Object.keys(soundTypeLabels).forEach((soundType) => {
			SoundActionCreators.updateSoundSettings({
				soundType: soundType as SoundType,
				enabled: true,
			});
		});
	};

	const handleDisableAllSounds = () => {
		Object.keys(soundTypeLabels).forEach((soundType) => {
			SoundActionCreators.updateSoundSettings({
				soundType: soundType as SoundType,
				enabled: false,
			});
		});
	};

	const handlePreviewSound = React.useCallback((soundType: SoundType) => {
		SoundActionCreators.stopAllSounds();
		SoundActionCreators.playSound(soundType);
	}, []);

	React.useEffect(() => {
		return () => {
			SoundActionCreators.stopAllSounds();
		};
	}, []);

	const handleCustomSoundUpload = React.useCallback(
		async (soundType: SoundType, file: File | null) => {
			if (!hasPremium || !file) {
				return;
			}
			const validation = CustomSoundDB.isValidAudioFile(file);
			if (!validation.valid) {
				ToastActionCreators.createToast({
					type: 'error',
					children: validation.error || t`Invalid audio file`,
				});
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
				ToastActionCreators.createToast({
					type: 'success',
					children: t`Custom sound uploaded successfully`,
				});
			} catch {
				ToastActionCreators.createToast({
					type: 'error',
					children: t`Failed to upload custom sound`,
				});
			}
		},
		[hasPremium],
	);

	const handleCustomSoundDelete = React.useCallback(async (soundType: SoundType) => {
		try {
			await CustomSoundDB.deleteCustomSound(soundType);
			clearCustomSoundCache(soundType);
			setCustomSounds((prev) => ({
				...prev,
				[soundType]: null,
			}));
			ToastActionCreators.createToast({
				type: 'success',
				children: t`Custom sound removed`,
			});
		} catch {
			ToastActionCreators.createToast({
				type: 'error',
				children: t`Failed to remove custom sound`,
			});
		}
	}, []);

	const handleUploadClick = React.useCallback(
		async (soundType: SoundType) => {
			if (!hasPremium) {
				PremiumModalActionCreators.open();
				return;
			}
			const [file] = await openFilePicker({accept: CustomSoundDB.SUPPORTED_MIME_TYPES.join(',')});
			await handleCustomSoundUpload(soundType, file ?? null);
		},
		[hasPremium, handleCustomSoundUpload],
	);

	return {
		hasPremium,
		soundTypeLabels,
		customSounds,
		handleToggleAllSounds,
		handleToggleSound,
		handleEnableAllSounds,
		handleDisableAllSounds,
		handlePreviewSound,
		handleUploadClick,
		handleCustomSoundDelete,
	};
};
