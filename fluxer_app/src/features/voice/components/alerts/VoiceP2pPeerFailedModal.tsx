// SPDX-License-Identifier: AGPL-3.0-or-later

import * as Modal from '@app/features/app/components/dialogs/Modal';
import {Button} from '@app/features/ui/button/Button';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {getCurrentLocale} from '@app/features/user/utils/LocaleUtils';
import {
	VOICE_P2P_LEAVE_DESCRIPTOR,
	VOICE_P2P_PEER_FAILED_DESCRIPTION_DESCRIPTOR,
	VOICE_P2P_PEER_FAILED_SWITCH_HINT_DESCRIPTOR,
	VOICE_P2P_PEER_FAILED_TITLE_DESCRIPTOR,
	VOICE_P2P_STAY_DESCRIPTOR,
	VOICE_P2P_SWITCH_TO_STANDARD_DESCRIPTOR,
} from '@app/features/voice/utils/VoiceMessageDescriptors';
import {useLingui} from '@lingui/react/macro';
import {formatListWithConfig} from '@pkgs/list_utils/src/ListFormatting';
import {observer} from 'mobx-react-lite';

export interface VoiceP2pPeerFailedModalProps {
	names: ReadonlyArray<string>;
	allowSwitch: boolean;
	onLeave: () => void;
	onSwitch: () => void;
}

export const VoiceP2pPeerFailedModal = observer(
	({names, allowSwitch, onLeave, onSwitch}: VoiceP2pPeerFailedModalProps) => {
		const {i18n} = useLingui();
		const formattedNames = formatListWithConfig(names, {
			locale: getCurrentLocale(),
			style: 'long',
			type: 'conjunction',
		});
		const handleLeave = () => {
			ModalCommands.pop();
			onLeave();
		};
		const handleSwitch = () => {
			ModalCommands.pop();
			onSwitch();
		};
		return (
			<Modal.Root size="small" centered data-flx="voice.voice-p2p-peer-failed-modal.modal-root">
				<Modal.Header
					title={i18n._(VOICE_P2P_PEER_FAILED_TITLE_DESCRIPTOR, {names: formattedNames})}
					data-flx="voice.voice-p2p-peer-failed-modal.modal-header"
				/>
				<Modal.Content data-flx="voice.voice-p2p-peer-failed-modal.modal-content">
					<Modal.ContentLayout data-flx="voice.voice-p2p-peer-failed-modal.modal-content-layout">
						<Modal.Description data-flx="voice.voice-p2p-peer-failed-modal.description">
							{allowSwitch
								? `${i18n._(VOICE_P2P_PEER_FAILED_DESCRIPTION_DESCRIPTOR)} ${i18n._(VOICE_P2P_PEER_FAILED_SWITCH_HINT_DESCRIPTOR)}`
								: i18n._(VOICE_P2P_PEER_FAILED_DESCRIPTION_DESCRIPTOR)}
						</Modal.Description>
					</Modal.ContentLayout>
				</Modal.Content>
				<Modal.Footer data-flx="voice.voice-p2p-peer-failed-modal.modal-footer">
					<Button
						variant="secondary"
						onClick={() => ModalCommands.pop()}
						data-flx="voice.voice-p2p-peer-failed-modal.button.stay"
					>
						{i18n._(VOICE_P2P_STAY_DESCRIPTOR)}
					</Button>
					<Button variant="danger" onClick={handleLeave} data-flx="voice.voice-p2p-peer-failed-modal.button.leave">
						{i18n._(VOICE_P2P_LEAVE_DESCRIPTOR)}
					</Button>
					{allowSwitch && (
						<Button variant="primary" onClick={handleSwitch} data-flx="voice.voice-p2p-peer-failed-modal.button.switch">
							{i18n._(VOICE_P2P_SWITCH_TO_STANDARD_DESCRIPTOR)}
						</Button>
					)}
				</Modal.Footer>
			</Modal.Root>
		);
	},
);
