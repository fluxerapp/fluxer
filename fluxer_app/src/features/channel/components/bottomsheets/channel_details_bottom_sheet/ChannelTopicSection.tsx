// SPDX-License-Identifier: AGPL-3.0-or-later

import styles from '@app/features/channel/components/bottomsheets/ChannelDetailsBottomSheet.module.css';
import {
	COLLAPSE_CHANNEL_TOPIC_DESCRIPTOR,
	EXPAND_CHANNEL_TOPIC_DESCRIPTOR,
} from '@app/features/channel/components/bottomsheets/channel_details_bottom_sheet/ChannelDetailsBottomSheetShared';
import {ChannelTopicModal} from '@app/features/channel/components/modals/ChannelTopicModal';
import {isKeyboardActivationKey} from '@app/features/input/utils/KeyboardUtils';
import {SafeMarkdown} from '@app/features/messaging/components/markdown';
import {MarkdownContext} from '@app/features/messaging/components/markdown/renderers/RendererTypes';
import markupStyles from '@app/features/theme/styles/Markup.module.css';
import {CollapseChevronIcon, ExpandChevronIcon} from '@app/features/ui/action_menu/ContextMenuIcons';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {modal} from '@app/features/ui/commands/ModalCommands';
import {useLingui} from '@lingui/react/macro';
import type React from 'react';
import {useState} from 'react';

interface ChannelTopicSectionProps {
	channelId: string;
	topic: string;
}

export const ChannelTopicSection: React.FC<ChannelTopicSectionProps> = ({channelId, topic}) => {
	const {i18n} = useLingui();
	const [isTopicExpanded, setIsTopicExpanded] = useState(false);
	const openTopicModal = () => {
		ModalCommands.push(
			modal(() => (
				<ChannelTopicModal channelId={channelId} data-flx="channel.channel-details-bottom-sheet.channel-topic-modal" />
			)),
		);
	};
	return (
		<div
			className={styles.topicSectionContainer}
			data-flx="channel.channel-details-bottom-sheet.topic-section-container"
		>
			<div className={styles.topicWrapper} data-flx="channel.channel-details-bottom-sheet.topic-wrapper">
				<div
					role="button"
					className={`${markupStyles.markup} ${markupStyles.mutedSpoilerContext} ${styles.topicMarkup} ${!isTopicExpanded ? styles.topicMarkupCollapsed : ''}`}
					style={
						isTopicExpanded
							? {
									wordWrap: 'break-word',
									overflowWrap: 'break-word',
									whiteSpace: 'break-spaces',
								}
							: undefined
					}
					onClick={openTopicModal}
					onKeyDown={(event) => {
						if (!isKeyboardActivationKey(event.key)) return;
						event.preventDefault();
						openTopicModal();
					}}
					tabIndex={0}
					data-flx="channel.channel-details-bottom-sheet.topic-markup.push"
				>
					<SafeMarkdown
						content={topic}
						options={{
							context: MarkdownContext.RESTRICTED_INLINE_REPLY,
							channelId,
							disableEmojiInfoCard: true,
						}}
						data-flx="channel.channel-details-bottom-sheet.safe-markdown"
					/>
				</div>
				<button
					type="button"
					onClick={() => setIsTopicExpanded(!isTopicExpanded)}
					className={styles.topicExpandButton}
					aria-label={
						isTopicExpanded ? i18n._(COLLAPSE_CHANNEL_TOPIC_DESCRIPTOR) : i18n._(EXPAND_CHANNEL_TOPIC_DESCRIPTOR)
					}
					aria-expanded={isTopicExpanded}
					data-flx="channel.channel-details-bottom-sheet.topic-expand-button.set-is-topic-expanded"
				>
					{isTopicExpanded ? (
						<CollapseChevronIcon
							className={styles.iconSmall}
							data-flx="channel.channel-details-bottom-sheet.icon-small"
						/>
					) : (
						<ExpandChevronIcon
							className={styles.iconSmall}
							data-flx="channel.channel-details-bottom-sheet.icon-small--2"
						/>
					)}
				</button>
			</div>
		</div>
	);
};
