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

import {Trans, useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import React from 'react';
import * as ModalActionCreators from '~/actions/ModalActionCreators';
import {GroupDMAvatar} from '~/components/common/GroupDMAvatar';
import {ConfirmModal} from '~/components/modals/ConfirmModal';
import FocusRing from '~/components/uikit/FocusRing/FocusRing';
import {Routes} from '~/Routes';
import type {ChannelRecord} from '~/records/ChannelRecord';
import * as ChannelUtils from '~/utils/ChannelUtils';
import * as DateUtils from '~/utils/DateUtils';
import * as RouterUtils from '~/utils/RouterUtils';
import * as SnowflakeUtils from '~/utils/SnowflakeUtils';
import styles from './DuplicateGroupConfirmModal.module.css';

interface DuplicateGroupConfirmModalProps {
	channels: Array<ChannelRecord>;
	onConfirm: () => Promise<void> | void;
}

export const DuplicateGroupConfirmModal = observer(({channels, onConfirm}: DuplicateGroupConfirmModalProps) => {
	const {t} = useLingui();
	const handleChannelClick = React.useCallback((channelId: string) => {
		ModalActionCreators.pop();
		RouterUtils.transitionTo(Routes.dmChannel(channelId));
	}, []);

	const description = React.useMemo(() => {
		return (
			<>
				<p className={styles.description}>
					<Trans>
						You already have a group with these users. Do you really want to create a new one? That&apos;s fine too!
					</Trans>
				</p>
				{channels.length > 0 && (
					<div className={styles.channelList}>
						{channels.map((channel) => {
							const lastActivitySnowflake = channel.lastMessageId ?? channel.id;
							const lastActiveText = DateUtils.getShortRelativeDateString(
								SnowflakeUtils.extractTimestamp(lastActivitySnowflake),
							);
							const lastActiveLabel = lastActiveText || t`No activity yet`;

							return (
								<FocusRing key={channel.id} offset={-2}>
									<button type="button" className={styles.channelItem} onClick={() => handleChannelClick(channel.id)}>
										<div className={styles.avatarWrapper}>
											<GroupDMAvatar channel={channel} size={40} />
										</div>
										<div className={styles.channelDetails}>
											<span className={styles.channelName}>{ChannelUtils.getDMDisplayName(channel)}</span>
											<span className={styles.lastActive}>{lastActiveLabel}</span>
										</div>
									</button>
								</FocusRing>
							);
						})}
					</div>
				)}
			</>
		);
	}, [channels, handleChannelClick]);

	return (
		<ConfirmModal
			title={t`Confirm New Group`}
			description={description}
			primaryText={t`Create new group`}
			primaryVariant="primary"
			secondaryText={t`Cancel`}
			size="small"
			onPrimary={onConfirm}
		/>
	);
});
