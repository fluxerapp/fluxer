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

import * as ModalActionCreators from '~/actions/ModalActionCreators';
import {modal} from '~/actions/ModalActionCreators';
import {ScreenRecordingPermissionDeniedModal} from '~/components/alerts/ScreenRecordingPermissionDeniedModal';
import {ScreenShareUnsupportedModal} from '~/components/alerts/ScreenShareUnsupportedModal';
import {ScreenRecordingPermissionDeniedError} from '~/utils/errors/ScreenRecordingPermissionDeniedError';

const isScreenShareUnsupportedError = (error: unknown): boolean => {
	if (!(error instanceof Error)) return false;

	return (
		error.name === 'DeviceUnsupportedError' ||
		error.message.includes('getDisplayMedia not supported') ||
		error.message.includes('NotSupportedError') ||
		error.message.includes('NotAllowedError')
	);
};

const handleScreenShareError = (error: unknown): void => {
	if (error instanceof ScreenRecordingPermissionDeniedError) {
		ModalActionCreators.push(modal(() => <ScreenRecordingPermissionDeniedModal />));
		return;
	}
	if (isScreenShareUnsupportedError(error)) {
		ModalActionCreators.push(modal(() => <ScreenShareUnsupportedModal />));
	} else {
		console.error('Failed to start screen share:', error);
	}
};

export const executeScreenShareOperation = async (
	operation: () => Promise<void>,
	onError?: (error: unknown) => void,
): Promise<void> => {
	try {
		await operation();
	} catch (error) {
		handleScreenShareError(error);
		if (onError) {
			onError(error);
		}
		throw error;
	}
};
