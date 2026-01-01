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
import * as ToastActionCreators from '~/actions/ToastActionCreators';
import * as CustomSoundDB from '~/utils/CustomSoundDB';
import {openFilePicker} from '~/utils/FilePickerUtils';

export const useEntranceSound = (hasPremium: boolean) => {
	const {t} = useLingui();
	const [entranceSound, setEntranceSound] = React.useState<CustomSoundDB.EntranceSound | null>(null);
	const [isPreviewing, setIsPreviewing] = React.useState(false);

	React.useEffect(() => {
		const loadEntranceSound = async () => {
			try {
				const sound = await CustomSoundDB.getEntranceSound();
				setEntranceSound(sound);
			} catch (error) {
				console.error('Failed to load entrance sound:', error);
			}
		};
		loadEntranceSound();
	}, []);

	const upload = React.useCallback(
		async (file: File | null) => {
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

			const durationValidation = await CustomSoundDB.validateAudioDuration(file);
			if (!durationValidation.valid) {
				ToastActionCreators.createToast({
					type: 'error',
					children: durationValidation.error || t`Audio is too long`,
				});
				return;
			}

			try {
				await CustomSoundDB.saveEntranceSound(file, file.name, durationValidation.duration!);
				const savedSound = await CustomSoundDB.getEntranceSound();
				setEntranceSound(savedSound);

				ToastActionCreators.createToast({
					type: 'success',
					children: t`Entrance sound uploaded successfully`,
				});
			} catch (error) {
				console.error('Failed to upload entrance sound:', error);
				ToastActionCreators.createToast({
					type: 'error',
					children: t`Failed to upload entrance sound`,
				});
			}
		},
		[hasPremium, t],
	);

	const remove = React.useCallback(async () => {
		try {
			await CustomSoundDB.deleteEntranceSound();
			setEntranceSound(null);

			ToastActionCreators.createToast({
				type: 'success',
				children: t`Entrance sound removed`,
			});
		} catch (error) {
			console.error('Failed to delete entrance sound:', error);
			ToastActionCreators.createToast({
				type: 'error',
				children: t`Failed to remove entrance sound`,
			});
		}
	}, [t]);

	const preview = React.useCallback(async () => {
		if (!entranceSound || isPreviewing) return;

		try {
			setIsPreviewing(true);
			const url = URL.createObjectURL(entranceSound.blob);
			const audio = new Audio(url);

			audio.onended = () => {
				URL.revokeObjectURL(url);
				setIsPreviewing(false);
			};

			audio.onerror = () => {
				URL.revokeObjectURL(url);
				setIsPreviewing(false);
			};

			await audio.play();
		} catch (error) {
			console.error('Failed to preview entrance sound:', error);
			setIsPreviewing(false);
		}
	}, [entranceSound, isPreviewing]);

	const openUploadDialog = React.useCallback(async () => {
		if (!hasPremium) {
			PremiumModalActionCreators.open();
			return;
		}
		const [file] = await openFilePicker({accept: CustomSoundDB.SUPPORTED_MIME_TYPES.join(',')});
		await upload(file ?? null);
	}, [hasPremium, upload]);

	return {
		entranceSound,
		isPreviewing,
		upload,
		remove,
		preview,
		openUploadDialog,
	};
};
