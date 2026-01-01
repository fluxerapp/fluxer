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

import type React from 'react';
import {useCallback} from 'react';
import * as ContextMenuActionCreators from '~/actions/ContextMenuActionCreators';
import {VoiceAudioSettingsMenu} from '~/components/voice/VoiceSettingsMenus';

interface UseAudioSettingsMenuOptions {
	inputDevices: Array<MediaDeviceInfo>;
	outputDevices: Array<MediaDeviceInfo>;
	isMobile?: boolean;
	onOpenMobile?: () => void;
}

interface UseAudioSettingsMenuResult {
	renderAudioSettingsMenu: (props: {onClose: () => void}) => React.ReactNode;
	handleAudioSettingsContextMenu: (event: React.MouseEvent) => void;
}

export const useAudioSettingsMenu = ({
	inputDevices,
	outputDevices,
	isMobile = false,
	onOpenMobile,
}: UseAudioSettingsMenuOptions): UseAudioSettingsMenuResult => {
	const renderAudioSettingsMenu = useCallback(
		({onClose}: {onClose: () => void}) => (
			<VoiceAudioSettingsMenu inputDevices={inputDevices} outputDevices={outputDevices} onClose={onClose} />
		),
		[inputDevices, outputDevices],
	);

	const handleAudioSettingsContextMenu = useCallback(
		(event: React.MouseEvent) => {
			if (isMobile) {
				event.preventDefault();
				event.stopPropagation();
				onOpenMobile?.();
				return;
			}

			ContextMenuActionCreators.openFromEvent(event, renderAudioSettingsMenu);
		},
		[isMobile, onOpenMobile, renderAudioSettingsMenu],
	);

	return {
		renderAudioSettingsMenu,
		handleAudioSettingsContextMenu,
	};
};
