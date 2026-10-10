// SPDX-License-Identifier: AGPL-3.0-or-later

import * as Modal from '@app/features/app/components/dialogs/Modal';
import {CANCEL_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {Button} from '@app/features/ui/button/Button';
import {Checkbox} from '@app/features/ui/checkbox/Checkbox';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import styles from '@app/features/voice/components/alerts/VoiceP2pConsentModal.module.css';
import VoiceP2pRollout from '@app/features/voice/state/VoiceP2pRollout';
import VoicePrompts from '@app/features/voice/state/VoicePrompts';
import {
	VOICE_P2P_ALWAYS_AGREE_DESCRIPTOR,
	VOICE_P2P_CONNECT_DIRECTLY_DESCRIPTOR,
	VOICE_P2P_DIRECT_MEDIA_DESCRIPTOR,
	VOICE_P2P_IP_ADDRESS_DESCRIPTOR,
	VOICE_P2P_JOIN_CONFIRM_DESCRIPTOR,
	VOICE_P2P_JOIN_TITLE_DESCRIPTOR,
	VOICE_P2P_LOWER_LATENCY_DESCRIPTOR,
	VOICE_P2P_MAX_PARTICIPANTS_DESCRIPTOR,
	VOICE_P2P_START_CONFIRM_DESCRIPTOR,
	VOICE_P2P_START_STANDARD_DESCRIPTOR,
	VOICE_P2P_START_TITLE_DESCRIPTOR,
	VOICE_P2P_STREAM_QUALITY_DESCRIPTOR,
} from '@app/features/voice/utils/VoiceMessageDescriptors';
import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import {useRef, useState} from 'react';

export interface VoiceP2pConsentModalProps {
	intent: 'start' | 'join';
	allowStandard: boolean;
	onP2p: () => void;
	onStandard?: () => void;
	onCancel: () => void;
}

export const VoiceP2pConsentModal = observer(
	({intent, allowStandard, onP2p, onStandard, onCancel}: VoiceP2pConsentModalProps) => {
		const {i18n} = useLingui();
		const [alwaysAgree, setAlwaysAgree] = useState(false);
		const initialFocusRef = useRef<HTMLButtonElement | null>(null);
		const isJoin = intent === 'join';
		const showStandard = !isJoin && allowStandard && onStandard != null;
		const handleP2p = () => {
			if (isJoin && alwaysAgree) VoicePrompts.setSkipP2pJoinConfirm(true);
			ModalCommands.pop();
			onP2p();
		};
		const handleStandard = () => {
			ModalCommands.pop();
			onStandard?.();
		};
		const handleCancel = () => {
			ModalCommands.pop();
			onCancel();
		};
		return (
			<Modal.Root
				size="small"
				centered
				initialFocusRef={initialFocusRef}
				onClose={handleCancel}
				data-flx="voice.voice-p2p-consent-modal.modal-root"
			>
				<Modal.Header
					title={i18n._(isJoin ? VOICE_P2P_JOIN_TITLE_DESCRIPTOR : VOICE_P2P_START_TITLE_DESCRIPTOR)}
					onClose={handleCancel}
					data-flx="voice.voice-p2p-consent-modal.modal-header"
				/>
				<Modal.Content data-flx="voice.voice-p2p-consent-modal.modal-content">
					<Modal.ContentLayout data-flx="voice.voice-p2p-consent-modal.modal-content-layout">
						<Modal.Description data-flx="voice.voice-p2p-consent-modal.description">
							{i18n._(VOICE_P2P_DIRECT_MEDIA_DESCRIPTOR)}
						</Modal.Description>
						<ul className={styles.points} data-flx="voice.voice-p2p-consent-modal.points">
							<li data-flx="voice.voice-p2p-consent-modal.point.latency">
								{i18n._(VOICE_P2P_LOWER_LATENCY_DESCRIPTOR)}
							</li>
							<li data-flx="voice.voice-p2p-consent-modal.point.quality">
								{i18n._(VOICE_P2P_STREAM_QUALITY_DESCRIPTOR)}
							</li>
							<li data-flx="voice.voice-p2p-consent-modal.point.ip-address">
								{i18n._(VOICE_P2P_IP_ADDRESS_DESCRIPTOR)}
							</li>
							<li data-flx="voice.voice-p2p-consent-modal.point.max-participants">
								{i18n._(VOICE_P2P_MAX_PARTICIPANTS_DESCRIPTOR, {maxParticipants: VoiceP2pRollout.maxParticipants})}
							</li>
							<li data-flx="voice.voice-p2p-consent-modal.point.connect-directly">
								{i18n._(VOICE_P2P_CONNECT_DIRECTLY_DESCRIPTOR)}
							</li>
						</ul>
						{isJoin && (
							<Checkbox
								checked={alwaysAgree}
								onChange={(checked) => setAlwaysAgree(checked)}
								size="small"
								data-flx="voice.voice-p2p-consent-modal.checkbox.always-agree"
							>
								<span className={styles.checkboxLabel} data-flx="voice.voice-p2p-consent-modal.checkbox-label">
									{i18n._(VOICE_P2P_ALWAYS_AGREE_DESCRIPTOR)}
								</span>
							</Checkbox>
						)}
					</Modal.ContentLayout>
				</Modal.Content>
				<Modal.Footer data-flx="voice.voice-p2p-consent-modal.modal-footer">
					<Button variant="secondary" onClick={handleCancel} data-flx="voice.voice-p2p-consent-modal.button.cancel">
						{i18n._(CANCEL_DESCRIPTOR)}
					</Button>
					{showStandard && (
						<Button
							variant="secondary"
							onClick={handleStandard}
							data-flx="voice.voice-p2p-consent-modal.button.standard"
						>
							{i18n._(VOICE_P2P_START_STANDARD_DESCRIPTOR)}
						</Button>
					)}
					<Button
						variant="primary"
						onClick={handleP2p}
						ref={initialFocusRef}
						data-flx="voice.voice-p2p-consent-modal.button.p2p"
					>
						{i18n._(isJoin ? VOICE_P2P_JOIN_CONFIRM_DESCRIPTOR : VOICE_P2P_START_CONFIRM_DESCRIPTOR)}
					</Button>
				</Modal.Footer>
			</Modal.Root>
		);
	},
);
