// SPDX-License-Identifier: AGPL-3.0-or-later

import * as Modal from '@app/features/app/components/dialogs/Modal';
import styles from '@app/features/channel/components/modals/ChannelTopicModal.module.css';
import {type ChannelTopicModalProps, getChannelTopicInfo} from '@app/features/channel/utils/ChannelTopicModalUtils';
import {SafeMarkdown} from '@app/features/messaging/components/markdown';
import {MarkdownContext} from '@app/features/messaging/components/markdown/renderers/RendererTypes';
import markupStyles from '@app/features/theme/styles/Markup.module.css';
import {clsx} from 'clsx';
import {observer} from 'mobx-react-lite';

export const ChannelTopicModal = observer(({channelId}: ChannelTopicModalProps) => {
	const topicInfo = getChannelTopicInfo(channelId);
	if (!topicInfo) {
		return null;
	}
	const {topic, title} = topicInfo;
	return (
		<Modal.Root size="small" centered data-flx="channel.channel-topic-modal.modal-root">
			<Modal.Header title={title} data-flx="channel.channel-topic-modal.modal-header" />
			<Modal.Content className={styles.selectable} data-flx="channel.channel-topic-modal.selectable">
				<Modal.ContentLayout data-flx="channel.channel-topic-modal.modal-content-layout">
					<div className={clsx(markupStyles.markup, styles.topic)} data-flx="channel.channel-topic-modal.topic">
						<SafeMarkdown
							content={topic}
							options={{
								context: MarkdownContext.STANDARD_WITHOUT_JUMBO,
								channelId,
								disableEmojiInfoCard: true,
							}}
							data-flx="channel.channel-topic-modal.safe-markdown"
						/>
					</div>
				</Modal.ContentLayout>
			</Modal.Content>
		</Modal.Root>
	);
});
