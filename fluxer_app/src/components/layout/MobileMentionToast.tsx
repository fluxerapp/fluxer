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

import type {I18n} from '@lingui/core';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {AnimatePresence, motion, type PanInfo} from 'framer-motion';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useEffect} from 'react';
import {MessageTypes} from '~/Constants';
import {SafeMarkdown} from '~/lib/markdown';
import {MarkdownContext} from '~/lib/markdown/renderers';
import type {MessageRecord} from '~/records/MessageRecord';
import ChannelStore from '~/stores/ChannelStore';
import GuildStore from '~/stores/GuildStore';
import MobileMentionToastStore from '~/stores/MobileMentionToastStore';
import * as ChannelUtils from '~/utils/ChannelUtils';
import {isMobileExperienceEnabled} from '~/utils/mobileExperience';
import {SystemMessageUtils} from '~/utils/SystemMessageUtils';
import styles from './MobileMentionToast.module.css';

const DISPLAY_DURATION_MS = 3000;

const getChannelLabel = (channelId: string, i18n: I18n): string => {
	const channel = ChannelStore.getChannel(channelId);
	if (!channel) {
		return i18n._(msg`Unknown channel`);
	}

	if (channel.isGuildText()) {
		const channelName = channel.name?.trim();
		const fallback = i18n._(msg`Unknown channel`);
		return channelName ? `#${channelName}` : fallback;
	}

	return ChannelUtils.getDMDisplayName(channel);
};

const getLocationLabel = (message: MessageRecord, i18n: I18n): string => {
	const channel = ChannelStore.getChannel(message.channelId);
	const channelLabel = getChannelLabel(message.channelId, i18n);

	if (channel?.guildId) {
		const guild = GuildStore.getGuild(channel.guildId);
		if (guild && channel.isGuildText()) {
			return `${guild.name} • ${channelLabel}`;
		}
	}

	return channelLabel;
};

const renderMessageContent = (message: MessageRecord, i18n: I18n): React.ReactNode => {
	if (message.type !== MessageTypes.DEFAULT && message.type !== MessageTypes.REPLY) {
		const systemText = SystemMessageUtils.stringify(message, i18n);
		if (systemText) {
			return <span className={styles.systemLabel}>{systemText.replace(/\.$/, '')}</span>;
		}
		return null;
	}

	if (message.content) {
		return (
			<div className={styles.messageContent}>
				<SafeMarkdown
					content={message.content}
					options={{
						context: MarkdownContext.RESTRICTED_INLINE_REPLY,
						channelId: message.channelId,
						messageId: message.id,
						disableAnimatedEmoji: true,
					}}
				/>
			</div>
		);
	}

	if (message.attachments.length > 0) {
		return <span className={styles.attachmentLabel}>{i18n._(msg`Sent an attachment`)}</span>;
	}

	return null;
};

export const MobileMentionToast = observer(() => {
	const {i18n} = useLingui();
	const current = MobileMentionToastStore.current;
	const isMobile = isMobileExperienceEnabled();

	useEffect(() => {
		if (!current || !isMobile) return;

		const timer = setTimeout(() => {
			MobileMentionToastStore.dequeue(current.id);
		}, DISPLAY_DURATION_MS);

		return () => clearTimeout(timer);
	}, [current?.id, isMobile]);

	if (!isMobile || !current) {
		return null;
	}

	const handleDragEnd = (_event: MouseEvent | TouchEvent | PointerEvent, info: PanInfo) => {
		if (Math.abs(info.offset.x) > 60) {
			MobileMentionToastStore.dequeue(current.id);
		}
	};

	const locationLabel = getLocationLabel(current, i18n);

	return (
		<div className={styles.host} role="status" aria-live="polite">
			<AnimatePresence initial={false} mode="popLayout">
				<motion.div
					key={current.id}
					className={styles.toast}
					initial={{opacity: 0, y: -10}}
					animate={{opacity: 1, y: 0}}
					exit={{opacity: 0, y: -10}}
					transition={{duration: 0.2, ease: 'easeOut'}}
					drag="x"
					dragConstraints={{left: 0, right: 0}}
					dragElastic={0.2}
					onDragEnd={handleDragEnd}
				>
					<div className={styles.header}>
						<span className={styles.author}>{current.author.displayName}</span>
						<span className={styles.separator} aria-hidden="true">
							•
						</span>
						<span className={styles.location}>{locationLabel}</span>
						<span className={styles.mentionLabel}>{i18n._(msg`Mentioned you`)}</span>
					</div>
					{renderMessageContent(current, i18n)}
					<div className={styles.progressTrack} aria-hidden="true">
						<motion.div
							key={current.id}
							className={styles.progressFill}
							initial={{scaleX: 1}}
							animate={{scaleX: 0}}
							transition={{duration: DISPLAY_DURATION_MS / 1000, ease: 'linear'}}
						/>
					</div>
				</motion.div>
			</AnimatePresence>
		</div>
	);
});
