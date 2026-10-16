// SPDX-License-Identifier: AGPL-3.0-or-later

import {ChannelPinsBottomSheet} from '@app/features/channel/components/bottomsheets/ChannelPinsBottomSheet';
import styles from '@app/features/channel/components/ChannelHeader.module.css';
import {ChannelHeaderIcon} from '@app/features/channel/components/channel_header_components/ChannelHeaderIcon';
import {ChannelPinsPopout} from '@app/features/channel/components/popouts/ChannelPinsPopout';
import type {Channel} from '@app/features/channel/models/Channel';
import ReadStates from '@app/features/read_state/state/ReadStates';
import {NotificationDot} from '@app/features/ui/components/NotificationDot';
import {usePopout} from '@app/features/ui/hooks/usePopout';
import {Popout} from '@app/features/ui/popover/PopoverPopout';
import MobileLayout from '@app/features/ui/state/MobileLayout';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {PushPinIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import {useCallback, useState} from 'react';

const PINNED_MESSAGES_UNREAD_PINNED_MESSAGES_DESCRIPTOR = msg({
	message: 'Pinned messages, unread pinned messages',
	comment: 'Button or menu action label in the channel pins button. Keep it concise.',
});
const PINNED_MESSAGES_DESCRIPTOR = msg({
	message: 'Pinned messages',
	comment: 'Button or menu action label in the channel pins button. Keep it concise.',
});

interface ChannelPinsButtonProps {
	channel: Channel;
}

export const ChannelPinsButton = observer(({channel}: ChannelPinsButtonProps) => {
	const {i18n} = useLingui();
	const {isOpen, openProps} = usePopout('channel-pins');
	const isMobile = MobileLayout.isMobileLayout();
	const [isBottomSheetOpen, setIsBottomSheetOpen] = useState(false);
	const hasUnreadPins = ReadStates.hasUnreadPins(channel.id);
	const pinsLabel = hasUnreadPins
		? i18n._(PINNED_MESSAGES_UNREAD_PINNED_MESSAGES_DESCRIPTOR)
		: i18n._(PINNED_MESSAGES_DESCRIPTOR);
	const handleClick = useCallback(() => {
		if (isMobile) {
			setIsBottomSheetOpen(true);
		}
	}, [isMobile]);
	const indicator = hasUnreadPins ? (
		<NotificationDot
			className={styles.unreadPinIndicator}
			data-flx="channel.channel-header-components.channel-pins-button.unread-pin-indicator"
		/>
	) : null;
	if (isMobile) {
		return (
			<>
				<div
					className={styles.iconButtonWrapper}
					data-flx="channel.channel-header-components.channel-pins-button.icon-button-wrapper"
				>
					<ChannelHeaderIcon
						icon={PushPinIcon}
						label={pinsLabel}
						isSelected={isBottomSheetOpen}
						aria-haspopup="dialog"
						aria-expanded={isBottomSheetOpen}
						onClick={handleClick}
						keybindAction="chat_toggle_pins"
						data-flx="channel.channel-header-components.channel-pins-button.channel-header-icon.click"
					/>
					{indicator}
				</div>
				<ChannelPinsBottomSheet
					isOpen={isBottomSheetOpen}
					onClose={() => setIsBottomSheetOpen(false)}
					channel={channel}
					data-flx="channel.channel-header-components.channel-pins-button.channel-pins-bottom-sheet"
				/>
			</>
		);
	}
	return (
		<Popout
			data-flx="channel.channel-header-components.channel-pins-button.popout"
			{...openProps}
			render={({onClose}) => (
				<ChannelPinsPopout
					channel={channel}
					onClose={onClose}
					data-flx="channel.channel-header-components.channel-pins-button.channel-pins-popout"
				/>
			)}
			position="bottom-end"
			subscribeTo="CHANNEL_PINS_OPEN"
		>
			<div
				className={styles.iconButtonWrapper}
				data-flx="channel.channel-header-components.channel-pins-button.icon-button-wrapper--2"
			>
				<ChannelHeaderIcon
					icon={PushPinIcon}
					label={pinsLabel}
					isSelected={isOpen}
					aria-haspopup={true}
					aria-expanded={isOpen}
					keybindAction="chat_toggle_pins"
					data-flx="channel.channel-header-components.channel-pins-button.channel-header-icon"
				/>
				{indicator}
			</div>
		</Popout>
	);
});
