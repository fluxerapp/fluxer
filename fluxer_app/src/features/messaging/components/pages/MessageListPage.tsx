// SPDX-License-Identifier: AGPL-3.0-or-later

import {Routes} from '@app/app/Routes';
import previewStyles from '@app/features/app/components/shared/MessagePreview.module.css';
import {ChannelHeader} from '@app/features/channel/components/ChannelHeader';
import {Message as MessageComponent} from '@app/features/channel/components/ChannelMessage';
import Channels from '@app/features/channel/state/Channels';
import {JUMP_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import styles from '@app/features/messaging/components/pages/MessageListPage.module.css';
import {useMessageListKeyboardNavigation} from '@app/features/messaging/hooks/useMessageListKeyboardNavigation';
import {useMessageSelectionCopyForMessages} from '@app/features/messaging/hooks/useMessageSelectionCopy';
import {NearViewportSurfaceContext} from '@app/features/messaging/hooks/useNearViewport';
import type {Message} from '@app/features/messaging/models/MessagingMessage';
import {focusChannelTextareaAfterNavigation} from '@app/features/messaging/utils/ChannelTextareaFocusUtils';
import {goToMessage} from '@app/features/messaging/utils/MessageNavigator';
import * as RouterUtils from '@app/features/navigation/utils/RouterUtils';
import {Scroller, type ScrollerHandle} from '@app/features/ui/components/Scroller';
import {MessagePreviewContext} from '@fluxer/constants/src/ChannelConstants';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {FlagCheckeredIcon, SparkleIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useMemo, useRef} from 'react';

const YOU_VE_REACHED_THE_END_DESCRIPTOR = msg({
	message: "You've reached the end",
	comment: 'Label in the message list page.',
});

interface MessageListPageProps {
	icon: React.ReactNode;
	title: string;
	messages: Array<Message>;
	emptyStateTitle: string;
	emptyStateDescription: string;
	endStateDescription: string;
	renderActionButtons: (message: Message) => React.ReactNode;
	renderMissingMessage?: (message: Message) => React.ReactNode;
}

export const MessageListPage = observer(
	({
		icon,
		title,
		messages,
		emptyStateTitle,
		emptyStateDescription,
		endStateDescription,
		renderActionButtons,
		renderMissingMessage,
	}: MessageListPageProps) => {
		const {i18n} = useLingui();
		const scrollerRef = useRef<ScrollerHandle | null>(null);
		const resolveListPageScrollSurface = useMemo(() => () => scrollerRef.current?.getViewportElement() ?? null, []);
		const onCopySelectedMessages = useMessageSelectionCopyForMessages<HTMLDivElement>(messages);
		const leftContent = (
			<div className={styles.header} data-flx="messaging.message-list-page.header">
				{icon}
				<span className={styles.title} data-flx="messaging.message-list-page.title">
					{title}
				</span>
			</div>
		);
		useMessageListKeyboardNavigation({
			containerRef: scrollerRef,
			allowWhenInactive: true,
		});
		return (
			<div className={styles.container} data-flx="messaging.message-list-page.container">
				<ChannelHeader
					leftContent={leftContent}
					showPins={false}
					data-flx="messaging.message-list-page.channel-header"
				/>
				<div className={styles.content} data-flx="messaging.message-list-page.content">
					{messages.length > 0 ? (
						<NearViewportSurfaceContext.Provider value={resolveListPageScrollSurface}>
							<Scroller
								className={styles.scroller}
								key="message-list-page-scroller"
								ref={scrollerRef}
								onCopy={onCopySelectedMessages}
								data-message-selection-root="true"
								data-flx="messaging.message-list-page.scroller"
							>
								{messages.map((message) => {
									const channel = Channels.getChannel(message.channelId);
									if (!channel) {
										if (renderMissingMessage) {
											return renderMissingMessage(message);
										}
										return null;
									}
									return (
										<div
											key={message.id}
											className={previewStyles.previewCard}
											data-message-id={message.id}
											data-is-group-start="true"
											data-flx="messaging.message-list-page.div"
										>
											<MessageComponent
												message={message}
												channel={channel}
												previewContext={MessagePreviewContext.LIST_POPOUT}
												data-flx="messaging.message-list-page.message-component"
											/>
											<div className={previewStyles.actionButtons} data-flx="messaging.message-list-page.div--2">
												<button
													type="button"
													className={previewStyles.actionButton}
													onClick={() => {
														const path = channel.guildId
															? Routes.guildChannel(channel.guildId, channel.id)
															: Routes.dmChannel(channel.id);
														RouterUtils.transitionTo(path);
														goToMessage(message.channelId, message.id);
														focusChannelTextareaAfterNavigation(message.channelId);
													}}
													data-flx="messaging.message-list-page.button.transition-to"
												>
													{i18n._(JUMP_DESCRIPTOR)}
												</button>
												{renderActionButtons(message)}
											</div>
										</div>
									);
								})}
								<div className={styles.endState} data-flx="messaging.message-list-page.end-state">
									<div className={styles.endStateContent} data-flx="messaging.message-list-page.end-state-content">
										<FlagCheckeredIcon
											className={styles.endStateIcon}
											data-flx="messaging.message-list-page.end-state-icon"
										/>
										<div className={styles.endStateText} data-flx="messaging.message-list-page.end-state-text">
											<h3 className={styles.endStateTitle} data-flx="messaging.message-list-page.end-state-title">
												{i18n._(YOU_VE_REACHED_THE_END_DESCRIPTOR)}
											</h3>
											<p
												className={styles.endStateDescription}
												data-flx="messaging.message-list-page.end-state-description"
											>
												{endStateDescription}
											</p>
										</div>
									</div>
								</div>
							</Scroller>
						</NearViewportSurfaceContext.Provider>
					) : (
						<div className={styles.emptyState} data-flx="messaging.message-list-page.empty-state">
							<div className={styles.emptyStateContent} data-flx="messaging.message-list-page.empty-state-content">
								<SparkleIcon
									className={styles.emptyStateIcon}
									data-flx="messaging.message-list-page.empty-state-icon"
								/>
								<div className={styles.emptyStateText} data-flx="messaging.message-list-page.empty-state-text">
									<h3 className={styles.emptyStateTitle} data-flx="messaging.message-list-page.empty-state-title">
										{emptyStateTitle}
									</h3>
									<p
										className={styles.emptyStateDescription}
										data-flx="messaging.message-list-page.empty-state-description"
									>
										{emptyStateDescription}
									</p>
								</div>
							</div>
						</div>
					)}
				</div>
			</div>
		);
	},
);
