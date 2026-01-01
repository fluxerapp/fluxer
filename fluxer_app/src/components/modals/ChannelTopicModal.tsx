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

import {clsx} from 'clsx';
import {observer} from 'mobx-react-lite';
import confirmStyles from '~/components/modals/ConfirmModal.module.css';
import * as Modal from '~/components/modals/Modal';
import {SafeMarkdown} from '~/lib/markdown';
import {MarkdownContext} from '~/lib/markdown/renderers';
import markupStyles from '~/styles/Markup.module.css';
import {type ChannelTopicModalProps, getChannelTopicInfo} from '~/utils/modals/ChannelTopicModalUtils';
import styles from './ChannelTopicModal.module.css';

export const ChannelTopicModal = observer(({channelId}: ChannelTopicModalProps) => {
	const topicInfo = getChannelTopicInfo(channelId);

	if (!topicInfo) {
		return null;
	}

	const {topic, title} = topicInfo;

	return (
		<Modal.Root size="small" centered>
			<Modal.Header title={title} />
			<Modal.Content className={clsx(confirmStyles.content, styles.selectable)}>
				<div className={clsx(markupStyles.markup, styles.topic)}>
					<SafeMarkdown
						content={topic}
						options={{
							context: MarkdownContext.STANDARD_WITHOUT_JUMBO,
							channelId,
						}}
					/>
				</div>
			</Modal.Content>
		</Modal.Root>
	);
});
