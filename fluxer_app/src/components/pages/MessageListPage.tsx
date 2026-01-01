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
import {FlagCheckeredIcon, SparkleIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {MessagePreviewContext} from '~/Constants';
import {ChannelHeader} from '~/components/channel/ChannelHeader';
import {Message} from '~/components/channel/Message';
import previewStyles from '~/components/shared/MessagePreview.module.css';
import {Scroller} from '~/components/uikit/Scroller';
import {Routes} from '~/Routes';
import type {MessageRecord} from '~/records/MessageRecord';
import ChannelStore from '~/stores/ChannelStore';
import {goToMessage} from '~/utils/MessageNavigator';
import * as RouterUtils from '~/utils/RouterUtils';
import styles from './MessageListPage.module.css';

interface MessageListPageProps {
	icon: React.ReactNode;
	title: string;
	messages: Array<MessageRecord>;
	emptyStateTitle: string;
	emptyStateDescription: string;
	endStateDescription: string;
	renderActionButtons: (message: MessageRecord) => React.ReactNode;
	renderMissingMessage?: (message: MessageRecord) => React.ReactNode;
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
		const {t} = useLingui();
		const leftContent = (
			<div className={styles.header}>
				{icon}
				<span className={styles.title}>{title}</span>
			</div>
		);

		return (
			<div className={styles.container}>
				<ChannelHeader leftContent={leftContent} showPins={false} />

				<div className={styles.content}>
					{messages.length > 0 ? (
						<Scroller className={styles.scroller} key="message-list-page-scroller">
							{messages.map((message) => {
								const channel = ChannelStore.getChannel(message.channelId);
								if (!channel) {
									if (renderMissingMessage) {
										return renderMissingMessage(message);
									}
									return null;
								}

								return (
									<div key={message.id} className={previewStyles.previewCard}>
										<Message message={message} channel={channel} previewContext={MessagePreviewContext.LIST_POPOUT} />
										<div className={previewStyles.actionButtons}>
											<button
												type="button"
												className={previewStyles.actionButton}
												onClick={() => {
													const path = channel.guildId
														? Routes.guildChannel(channel.guildId, channel.id)
														: Routes.dmChannel(channel.id);
													RouterUtils.transitionTo(path);
													goToMessage(message.channelId, message.id);
												}}
											>
												{t`Jump`}
											</button>

											{renderActionButtons(message)}
										</div>
									</div>
								);
							})}

							<div className={styles.endState}>
								<div className={styles.endStateContent}>
									<FlagCheckeredIcon className={styles.endStateIcon} />
									<div className={styles.endStateText}>
										<h3 className={styles.endStateTitle}>{t`You've reached the end`}</h3>
										<p className={styles.endStateDescription}>{endStateDescription}</p>
									</div>
								</div>
							</div>
						</Scroller>
					) : (
						<div className={styles.emptyState}>
							<div className={styles.emptyStateContent}>
								<SparkleIcon className={styles.emptyStateIcon} />
								<div className={styles.emptyStateText}>
									<h3 className={styles.emptyStateTitle}>{emptyStateTitle}</h3>
									<p className={styles.emptyStateDescription}>{emptyStateDescription}</p>
								</div>
							</div>
						</div>
					)}
				</div>
			</div>
		);
	},
);
