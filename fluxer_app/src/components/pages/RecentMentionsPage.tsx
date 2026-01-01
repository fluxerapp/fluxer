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
import {AtIcon, XIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import React from 'react';
import * as RecentMentionActionCreators from '~/actions/RecentMentionActionCreators';
import {MessageListPage} from '~/components/pages/MessageListPage';
import previewStyles from '~/components/shared/MessagePreview.module.css';
import type {MessageRecord} from '~/records/MessageRecord';
import RecentMentionsStore from '~/stores/RecentMentionsStore';
import styles from './RecentMentionsPage.module.css';

export const RecentMentionsPage = observer(() => {
	const {t} = useLingui();
	const recentMentions = RecentMentionsStore.recentMentions;
	const fetched = RecentMentionsStore.fetched;

	React.useEffect(() => {
		if (!fetched) {
			RecentMentionActionCreators.fetch();
		}
	}, [fetched]);

	const renderActionButtons = (message: MessageRecord) => (
		<button
			type="button"
			className={previewStyles.actionIconButton}
			onClick={() => RecentMentionActionCreators.remove(message.id)}
		>
			<XIcon weight="regular" className={previewStyles.actionIcon} />
		</button>
	);

	return (
		<MessageListPage
			icon={<AtIcon weight="bold" className={styles.icon} />}
			title={t`Recent Mentions`}
			messages={recentMentions.slice()}
			emptyStateTitle={t`No Recent Mentions`}
			emptyStateDescription={t`All @mentions of you will appear here for 7 days.`}
			endStateDescription={t`You've seen all your recent mentions. Don't fret, more will appear here soon`}
			renderActionButtons={renderActionButtons}
		/>
	);
});
