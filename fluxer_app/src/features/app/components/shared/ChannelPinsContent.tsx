// SPDX-License-Identifier: AGPL-3.0-or-later

import {ConfirmModal} from '@app/features/app/components/dialogs/ConfirmModal';
import {LongPressable} from '@app/features/app/components/LongPressable';
import previewStyles from '@app/features/app/components/shared/MessagePreview.module.css';
import * as ChannelPinCommands from '@app/features/channel/commands/ChannelPinsCommands';
import {Message as MessageComponent} from '@app/features/channel/components/ChannelMessage';
import type {Channel} from '@app/features/channel/models/Channel';
import ChannelPins from '@app/features/channel/state/ChannelPins';
import Channels from '@app/features/channel/state/Channels';
import {JUMP_DESCRIPTOR, UNPIN_MESSAGE_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {isKeyboardActivationKey} from '@app/features/input/utils/KeyboardUtils';
import {ensureMembersForMessages} from '@app/features/messaging/commands/MessageCommands';
import {useMessageListKeyboardNavigation} from '@app/features/messaging/hooks/useMessageListKeyboardNavigation';
import {useMessageSelectionCopyForMessages} from '@app/features/messaging/hooks/useMessageSelectionCopy';
import {NearViewportSurfaceContext} from '@app/features/messaging/hooks/useNearViewport';
import type {Message} from '@app/features/messaging/models/MessagingMessage';
import {focusChannelTextareaAfterNavigation} from '@app/features/messaging/utils/ChannelTextareaFocusUtils';
import {goToMessage} from '@app/features/messaging/utils/MessageNavigator';
import Permission from '@app/features/permissions/state/Permission';
import {formatPermissionLabel} from '@app/features/permissions/utils/PermissionUtils';
import ReadStates from '@app/features/read_state/state/ReadStates';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {modal} from '@app/features/ui/commands/ModalCommands';
import {Scroller, type ScrollerHandle} from '@app/features/ui/components/Scroller';
import {Spinner} from '@app/features/ui/components/Spinner';
import FocusRing from '@app/features/ui/focus_ring/FocusRing';
import {MenuBottomSheet} from '@app/features/ui/menu_bottom_sheet/MenuBottomSheet';
import MobileLayout from '@app/features/ui/state/MobileLayout';
import {ChannelTypes, MessagePreviewContext, Permissions} from '@fluxer/constants/src/ChannelConstants';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {FlagCheckeredIcon, PushPinSlashIcon, SparkleIcon, XIcon} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useCallback, useEffect, useMemo, useRef, useState} from 'react';

const DO_YOU_WANT_TO_SEND_THIS_PIN_BACK_DESCRIPTOR = msg({
	message: 'Send this pin back in time?',
	comment: 'Unpin confirmation body. Keep the time-travel quip.',
});
const UNPIN_IT_DESCRIPTOR = msg({
	message: 'Unpin it',
	comment: 'Short label in the shared app channel pins content.',
});
const MEMBERS_WITH_THE_PERMISSION_CAN_PIN_MESSAGES_FOR_DESCRIPTOR = msg({
	message: 'Members with "{pinMessagesPermissionLabel}" can pin messages for everyone.',
	comment: 'Channel pins empty-state body shown to community members. Pin permission name is interpolated.',
});
const YOU_CAN_PIN_MESSAGES_IN_THIS_CONVERSATION_FOR_DESCRIPTOR = msg({
	message: 'Pin messages here for everyone to see.',
	comment: 'Channel pins empty-state body shown in DMs/group DMs where the viewer can pin.',
});
const NO_PINNED_MESSAGES_DESCRIPTOR = msg({
	message: 'No pinned messages',
	comment: 'Short label in the shared app channel pins content.',
});
const WHENEVER_SOMEONE_PINS_A_MESSAGE_IT_LL_APPEAR_DESCRIPTOR = msg({
	message: 'Pinned messages show up here.',
	comment: 'Channel pins empty-state secondary line explaining where pinned messages appear.',
});
const YOU_VE_REACHED_THE_END_DESCRIPTOR = msg({
	message: "You've reached the end",
	comment: 'Short label in the shared app channel pins content.',
});

interface ChannelPinsContentProps {
	channel: Channel;
	onJump?: () => void;
}

export const ChannelPinsContent = observer(({channel, onJump}: ChannelPinsContentProps) => {
	const {i18n} = useLingui();
	const pinnedPins = ChannelPins.getPins(channel.id);
	const fetched = ChannelPins.isFetched(channel.id);
	const hasMore = ChannelPins.getHasMore(channel.id);
	const isLoading = ChannelPins.getIsLoading(channel.id);
	const isDMChannel = channel.type === ChannelTypes.DM || channel.type === ChannelTypes.GROUP_DM;
	const canUnpin = isDMChannel || Permission.can(Permissions.MANAGE_MESSAGES, channel);
	const mobileLayout = MobileLayout;
	const scrollerRef = useRef<ScrollerHandle | null>(null);
	const resolvePinnedScrollSurface = useMemo(() => () => scrollerRef.current?.getViewportElement() ?? null, []);
	const [menuOpen, setMenuOpen] = useState(false);
	const [selectedMessage, setSelectedMessage] = useState<Message | null>(null);
	const pinMessagesPermissionLabel = formatPermissionLabel(i18n, Permissions.PIN_MESSAGES);
	const pinnedMessages = useMemo(() => pinnedPins.map(({message}) => message), [pinnedPins]);
	const onCopySelectedMessages = useMessageSelectionCopyForMessages<HTMLDivElement>(pinnedMessages);
	useEffect(() => {
		if (!fetched && !isLoading) {
			ChannelPinCommands.fetch(channel.id);
		}
	}, [fetched, isLoading, channel.id]);
	useEffect(() => {
		ReadStates.ackPins(channel.id);
	}, [channel.id]);
	useEffect(() => {
		if (pinnedMessages.length === 0) return;
		void ensureMembersForMessages(pinnedMessages);
	}, [pinnedMessages]);
	useMessageListKeyboardNavigation({
		containerRef: scrollerRef,
	});
	const handleScroll = useCallback(
		(event: React.UIEvent<HTMLDivElement>) => {
			const target = event.currentTarget;
			const scrollPercentage = (target.scrollTop + target.offsetHeight) / target.scrollHeight;
			if (scrollPercentage > 0.8 && hasMore && !isLoading) {
				ChannelPinCommands.loadMore(channel.id);
			}
		},
		[channel.id, hasMore, isLoading],
	);
	const handleUnpin = (message: Message, event?: React.MouseEvent) => {
		if (event?.shiftKey) {
			ChannelPinCommands.unpin(message.channelId, message.id);
		} else {
			ModalCommands.push(
				modal(() => (
					<ConfirmModal
						title={i18n._(UNPIN_MESSAGE_DESCRIPTOR)}
						description={i18n._(DO_YOU_WANT_TO_SEND_THIS_PIN_BACK_DESCRIPTOR)}
						message={message}
						primaryText={i18n._(UNPIN_IT_DESCRIPTOR)}
						onPrimary={() => ChannelPinCommands.unpin(message.channelId, message.id)}
						data-flx="app.channel-pins-content.handle-unpin.confirm-modal"
					/>
				)),
			);
		}
		setMenuOpen(false);
	};
	const renderUnpinButton = (message: Message) => {
		if (!canUnpin) return null;
		return (
			<FocusRing offset={-2} data-flx="app.channel-pins-content.render-unpin-button.focus-ring">
				<button
					type="button"
					className={previewStyles.actionIconButton}
					onClick={(event) => handleUnpin(message, event)}
					aria-label={i18n._(UNPIN_MESSAGE_DESCRIPTOR)}
					data-flx="app.channel-pins-content.render-unpin-button.button.unpin"
				>
					<XIcon
						weight="bold"
						className={previewStyles.actionIcon}
						data-flx="app.channel-pins-content.render-unpin-button.x-icon"
					/>
				</button>
			</FocusRing>
		);
	};
	const handleJump = (channelId: string, messageId: string) => {
		goToMessage(channelId, messageId);
		onJump?.();
		focusChannelTextareaAfterNavigation(channelId);
	};
	const handleTap = (message: Message) => {
		if (mobileLayout.enabled) {
			handleJump(message.channelId, message.id);
		}
	};
	const endStateDescription = channel.guildId
		? i18n._(MEMBERS_WITH_THE_PERMISSION_CAN_PIN_MESSAGES_FOR_DESCRIPTOR, {pinMessagesPermissionLabel})
		: i18n._(YOU_CAN_PIN_MESSAGES_IN_THIS_CONVERSATION_FOR_DESCRIPTOR);
	if (!fetched) {
		return (
			<div className={previewStyles.emptyState} data-flx="app.channel-pins-content.div">
				<Spinner data-flx="app.channel-pins-content.spinner" />
			</div>
		);
	}
	if (pinnedPins.length === 0) {
		return (
			<div className={previewStyles.emptyState} data-flx="app.channel-pins-content.div--2">
				<div className={previewStyles.emptyStateContent} data-flx="app.channel-pins-content.div--3">
					<SparkleIcon className={previewStyles.emptyStateIcon} data-flx="app.channel-pins-content.sparkle-icon" />
					<div className={previewStyles.emptyStateTextContainer} data-flx="app.channel-pins-content.div--4">
						<h3 className={previewStyles.emptyStateTitle} data-flx="app.channel-pins-content.h3">
							{i18n._(NO_PINNED_MESSAGES_DESCRIPTOR)}
						</h3>
						<p className={previewStyles.emptyStateDescription} data-flx="app.channel-pins-content.p">
							{i18n._(WHENEVER_SOMEONE_PINS_A_MESSAGE_IT_LL_APPEAR_DESCRIPTOR)}
						</p>
					</div>
				</div>
			</div>
		);
	}
	return (
		<>
			<NearViewportSurfaceContext.Provider value={resolvePinnedScrollSurface}>
				<Scroller
					className={clsx(previewStyles.scroller, mobileLayout.enabled && previewStyles.scrollerMobile)}
					key="channel-pins-scroller"
					onScroll={handleScroll}
					ref={scrollerRef}
					onCopy={onCopySelectedMessages}
					data-message-selection-root="true"
					data-flx="app.channel-pins-content.scroller"
				>
					{mobileLayout.enabled && (
						<div className={previewStyles.topSpacer} data-flx="app.channel-pins-content.div--5" />
					)}
					{pinnedPins.slice().map(({message}) => {
						const cardClasses = clsx(
							previewStyles.previewCard,
							mobileLayout.enabled && previewStyles.previewCardMobile,
						);
						const messageChannel =
							Channels.getChannel(message.channelId) ?? (message.channelId === channel.id ? channel : null);
						if (!messageChannel) return null;
						if (mobileLayout.enabled) {
							return (
								<LongPressable
									key={message.id}
									className={cardClasses}
									data-message-id={message.id}
									data-is-group-start="true"
									role="button"
									tabIndex={0}
									onClick={() => handleTap(message)}
									onKeyDown={(e) => {
										if (isKeyboardActivationKey(e.key)) {
											e.preventDefault();
											handleTap(message);
										}
									}}
									onLongPress={() => {
										if (!canUnpin) return;
										setSelectedMessage(message);
										setMenuOpen(true);
									}}
									data-flx="app.channel-pins-content.button.tap"
								>
									<MessageComponent
										message={message}
										channel={messageChannel}
										previewContext={MessagePreviewContext.LIST_POPOUT}
										data-flx="app.channel-pins-content.message-component"
									/>
								</LongPressable>
							);
						}
						return (
							<div
								key={message.id}
								className={cardClasses}
								data-message-id={message.id}
								data-is-group-start="true"
								data-flx="app.channel-pins-content.div--6"
							>
								<MessageComponent
									message={message}
									channel={messageChannel}
									previewContext={MessagePreviewContext.LIST_POPOUT}
									data-flx="app.channel-pins-content.message-component--2"
								/>
								<div className={previewStyles.actionButtons} data-flx="app.channel-pins-content.div--7">
									<FocusRing offset={-2} data-flx="app.channel-pins-content.focus-ring">
										<button
											type="button"
											className={previewStyles.actionButton}
											onClick={() => handleJump(message.channelId, message.id)}
											data-flx="app.channel-pins-content.button.jump"
										>
											{i18n._(JUMP_DESCRIPTOR)}
										</button>
									</FocusRing>
									{renderUnpinButton(message)}
								</div>
							</div>
						);
					})}
					{isLoading && (
						<div className={previewStyles.loadingState} data-flx="app.channel-pins-content.div--8">
							<Spinner data-flx="app.channel-pins-content.spinner--2" />
						</div>
					)}
					{!hasMore && (
						<div className={previewStyles.endState} data-flx="app.channel-pins-content.div--9">
							<div className={previewStyles.endStateContent} data-flx="app.channel-pins-content.div--10">
								<FlagCheckeredIcon
									className={previewStyles.endStateIcon}
									data-flx="app.channel-pins-content.flag-checkered-icon"
								/>
								<div className={previewStyles.endStateTextContainer} data-flx="app.channel-pins-content.div--11">
									<h3 className={previewStyles.endStateTitle} data-flx="app.channel-pins-content.h3--2">
										{i18n._(YOU_VE_REACHED_THE_END_DESCRIPTOR)}
									</h3>
									<p className={previewStyles.endStateDescription} data-flx="app.channel-pins-content.p--2">
										{endStateDescription}
									</p>
								</div>
							</div>
						</div>
					)}
				</Scroller>
			</NearViewportSurfaceContext.Provider>
			{mobileLayout.enabled && selectedMessage && (
				<MenuBottomSheet
					isOpen={menuOpen}
					onClose={() => {
						setMenuOpen(false);
						setSelectedMessage(null);
					}}
					groups={[
						{
							items: [
								{
									icon: (
										<PushPinSlashIcon
											className={previewStyles.menuIcon}
											data-flx="app.channel-pins-content.push-pin-slash-icon"
										/>
									),
									label: i18n._(UNPIN_MESSAGE_DESCRIPTOR),
									onClick: () => handleUnpin(selectedMessage),
									danger: true,
								},
							],
						},
					]}
					data-flx="app.channel-pins-content.menu-bottom-sheet"
				/>
			)}
		</>
	);
});
