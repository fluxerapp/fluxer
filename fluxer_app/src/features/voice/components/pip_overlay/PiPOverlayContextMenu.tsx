// SPDX-License-Identifier: AGPL-3.0-or-later

import {remFromPx} from '@app/features/theme/layout/RemFromPx';
import {MenuGroup} from '@app/features/ui/action_menu/MenuGroup';
import {MenuItem} from '@app/features/ui/action_menu/MenuItem';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {modal} from '@app/features/ui/commands/ModalCommands';
import {DisableScreenSharePiPConfirmModal} from '@app/features/voice/components/dialogs/DisableScreenSharePiPConfirmModal';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {PictureInPictureIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import {useCallback} from 'react';

const DISABLE_SCREEN_SHARE_PIP_DESCRIPTOR = msg({
	message: 'Disable screen share picture-in-picture popouts',
	comment: 'Context menu item on the screen share PiP overlay that disables future screen share PiP popouts.',
});

export const PiPOverlayContextMenu = observer(function PiPOverlayContextMenu() {
	const {i18n} = useLingui();
	const handleDisableScreenSharePiP = useCallback(() => {
		ModalCommands.push(
			modal(() => (
				<DisableScreenSharePiPConfirmModal data-flx="voice.pi-p-overlay.context-menu.disable-screen-share-pip.confirm-modal" />
			)),
		);
	}, []);

	return (
		<MenuGroup data-flx="voice.pi-p-overlay.context-menu.menu-group">
			<MenuItem
				icon={
					<PictureInPictureIcon
						size={remFromPx(16)}
						weight="fill"
						data-flx="voice.pi-p-overlay.context-menu.disable-screen-share-pip.icon"
					/>
				}
				onClick={handleDisableScreenSharePiP}
				data-flx="voice.pi-p-overlay.context-menu.disable-screen-share-pip.menu-item"
			>
				{i18n._(DISABLE_SCREEN_SHARE_PIP_DESCRIPTOR)}
			</MenuItem>
		</MenuGroup>
	);
});
