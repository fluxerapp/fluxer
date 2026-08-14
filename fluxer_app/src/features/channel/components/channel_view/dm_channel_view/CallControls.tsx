// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	type CallControlRenderMode,
	CONNECTING_2_DESCRIPTOR,
	JOIN_CALL_DESCRIPTOR,
	JOIN_ON_THIS_DEVICE_DESCRIPTOR,
} from '@app/features/channel/components/channel_view/dm_channel_view/shared';
import {remFromPx} from '@app/features/theme/layout/RemFromPx';
import {Button} from '@app/features/ui/button/Button';
import {
	INCOMING_CALL_ACCEPT_ACTION_DESCRIPTOR,
	INCOMING_CALL_IGNORE_ACTION_DESCRIPTOR,
	INCOMING_CALL_REJECT_ACTION_DESCRIPTOR,
} from '@app/features/voice/utils/VoiceMessageDescriptors';
import {useLingui} from '@lingui/react/macro';
import {BellSlashIcon, PhoneIcon, XIcon} from '@phosphor-icons/react';
import type React from 'react';

type ControlsVariant = 'incoming' | 'join' | 'connecting' | 'inCall' | 'hidden';

interface CallControlsProps {
	mode: CallControlRenderMode;
	controlsVariant: ControlsVariant;
	currentChannelId: string | null;
	isDeviceInRoomForChannelCall: boolean;
	onJoinCall: () => void;
	onRejectIncomingCall: () => void;
	onIgnoreIncomingCall: () => void;
}

interface CallControlButtonProps {
	label: string;
	icon: React.ReactNode;
	onClick?: () => void;
	disabled?: boolean;
	submitting?: boolean;
	variant?: 'primary' | 'secondary' | 'danger';
}

const CallControlButton: React.FC<CallControlButtonProps> = ({
	label,
	icon,
	onClick,
	disabled = false,
	submitting = false,
	variant = 'secondary',
}) => (
	<Button
		variant={variant}
		leftIcon={icon}
		onClick={onClick}
		disabled={disabled}
		submitting={submitting}
		data-flx="channel.channel-view.dm-channel-view.render-secondary-call-button.button.click"
	>
		{label}
	</Button>
);
export const CallControls: React.FC<CallControlsProps> = ({
	mode,
	controlsVariant,
	currentChannelId,
	isDeviceInRoomForChannelCall,
	onJoinCall,
	onRejectIncomingCall,
	onIgnoreIncomingCall,
}) => {
	const {i18n} = useLingui();
	const useVoiceControlBarStyle = mode === 'voiceControlBar';
	if (controlsVariant === 'incoming') {
		if (useVoiceControlBarStyle) {
			return (
				<>
					<CallControlButton
						label={i18n._(INCOMING_CALL_ACCEPT_ACTION_DESCRIPTOR)}
						icon={
							<PhoneIcon
								size={remFromPx(16)}
								weight="fill"
								data-flx="channel.channel-view.dm-channel-view.render-call-controls.phone-icon"
							/>
						}
						onClick={onJoinCall}
						data-flx="channel.channel-view.dm-channel-view.call-controls.call-control-button.join-call"
					/>
					<CallControlButton
						label={i18n._(INCOMING_CALL_REJECT_ACTION_DESCRIPTOR)}
						icon={
							<XIcon
								size={remFromPx(18)}
								weight="bold"
								data-flx="channel.channel-view.dm-channel-view.render-call-controls.x-icon"
							/>
						}
						variant="danger"
						onClick={onRejectIncomingCall}
						data-flx="channel.channel-view.dm-channel-view.call-controls.call-control-button.reject-incoming-call"
					/>
					<CallControlButton
						label={i18n._(INCOMING_CALL_IGNORE_ACTION_DESCRIPTOR)}
						icon={
							<BellSlashIcon
								size={remFromPx(18)}
								weight="fill"
								data-flx="channel.channel-view.dm-channel-view.render-call-controls.bell-slash-icon"
							/>
						}
						onClick={onIgnoreIncomingCall}
						data-flx="channel.channel-view.dm-channel-view.call-controls.call-control-button.ignore-incoming-call"
					/>
				</>
			);
		}
		return (
			<>
				<Button
					variant="secondary"
					leftIcon={
						<PhoneIcon
							size={remFromPx(16)}
							weight="fill"
							data-flx="channel.channel-view.dm-channel-view.render-call-controls.phone-icon--2"
						/>
					}
					onClick={onJoinCall}
					data-flx="channel.channel-view.dm-channel-view.render-call-controls.button.join-call"
				>
					{i18n._(INCOMING_CALL_ACCEPT_ACTION_DESCRIPTOR)}
				</Button>
				<Button
					variant="danger"
					leftIcon={
						<XIcon
							size={remFromPx(16)}
							weight="bold"
							data-flx="channel.channel-view.dm-channel-view.render-call-controls.x-icon--2"
						/>
					}
					onClick={onRejectIncomingCall}
					data-flx="channel.channel-view.dm-channel-view.render-call-controls.button.reject-incoming-call"
				>
					{i18n._(INCOMING_CALL_REJECT_ACTION_DESCRIPTOR)}
				</Button>
				<Button
					variant="secondary"
					onClick={onIgnoreIncomingCall}
					data-flx="channel.channel-view.dm-channel-view.render-call-controls.button.ignore-incoming-call"
				>
					{i18n._(INCOMING_CALL_IGNORE_ACTION_DESCRIPTOR)}
				</Button>
			</>
		);
	}
	if (controlsVariant === 'join') {
		if (useVoiceControlBarStyle) {
			return (
				<CallControlButton
					label={i18n._(JOIN_CALL_DESCRIPTOR)}
					icon={
						<PhoneIcon
							size={remFromPx(16)}
							weight="fill"
							data-flx="channel.channel-view.dm-channel-view.render-call-controls.phone-icon--3"
						/>
					}
					onClick={onJoinCall}
					disabled={!currentChannelId}
					variant="primary"
					data-flx="channel.channel-view.dm-channel-view.call-controls.call-control-button.join-call--2"
				/>
			);
		}
		return (
			<Button
				variant="secondary"
				leftIcon={
					<PhoneIcon
						size={remFromPx(16)}
						weight="fill"
						data-flx="channel.channel-view.dm-channel-view.render-call-controls.phone-icon--4"
					/>
				}
				onClick={onJoinCall}
				disabled={!currentChannelId}
				data-flx="channel.channel-view.dm-channel-view.render-call-controls.button.join-call--2"
			>
				{i18n._(JOIN_CALL_DESCRIPTOR)}
			</Button>
		);
	}
	if (controlsVariant === 'connecting') {
		if (useVoiceControlBarStyle) {
			return (
				<CallControlButton
					label={i18n._(CONNECTING_2_DESCRIPTOR)}
					icon={
						<PhoneIcon
							size={remFromPx(16)}
							weight="fill"
							data-flx="channel.channel-view.dm-channel-view.render-call-controls.phone-icon--5"
						/>
					}
					submitting
					data-flx="channel.channel-view.dm-channel-view.call-controls.call-control-button"
				/>
			);
		}
		return (
			<Button
				variant="secondary"
				leftIcon={
					<PhoneIcon
						size={remFromPx(16)}
						weight="fill"
						data-flx="channel.channel-view.dm-channel-view.render-call-controls.phone-icon--6"
					/>
				}
				submitting
				data-flx="channel.channel-view.dm-channel-view.render-call-controls.button"
			>
				{i18n._(JOIN_CALL_DESCRIPTOR)}
			</Button>
		);
	}
	if (controlsVariant === 'inCall' && !isDeviceInRoomForChannelCall) {
		if (useVoiceControlBarStyle) {
			return (
				<CallControlButton
					label={i18n._(JOIN_ON_THIS_DEVICE_DESCRIPTOR)}
					icon={
						<PhoneIcon
							size={remFromPx(16)}
							weight="fill"
							data-flx="channel.channel-view.dm-channel-view.render-call-controls.phone-icon--7"
						/>
					}
					onClick={onJoinCall}
					disabled={!currentChannelId}
					data-flx="channel.channel-view.dm-channel-view.call-controls.call-control-button.join-call--3"
				/>
			);
		}
		return (
			<Button
				variant="secondary"
				leftIcon={
					<PhoneIcon
						size={remFromPx(16)}
						weight="fill"
						data-flx="channel.channel-view.dm-channel-view.render-call-controls.phone-icon--8"
					/>
				}
				onClick={onJoinCall}
				disabled={!currentChannelId}
				data-flx="channel.channel-view.dm-channel-view.render-call-controls.button.join-call--3"
			>
				{i18n._(JOIN_ON_THIS_DEVICE_DESCRIPTOR)}
			</Button>
		);
	}
	return null;
};
