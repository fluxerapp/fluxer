// SPDX-License-Identifier: AGPL-3.0-or-later

import {AddFriendForm} from '@app/features/channel/components/direct_message/AddFriendForm';
import styles from '@app/features/channel/components/direct_message/AddFriendView.module.css';
import {ADD_FRIEND_DESCRIPTOR} from '@app/features/relationship/utils/RelationshipMessageDescriptors';
import Users from '@app/features/user/state/Users';
import {Trans, useLingui} from '@lingui/react/macro';
import {UserCirclePlusIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';

export const AddFriendView = observer(() => {
	const {i18n} = useLingui();
	const showVerificationSlateOnly = Users.currentUser?.verified === false;
	return (
		<div
			className={styles.friendRequestContainer}
			data-flx="channel.direct-message.add-friend-view.add-friend-container"
		>
			<div className={styles.card} data-flx="channel.direct-message.add-friend-view.card">
				{showVerificationSlateOnly ? (
					<AddFriendForm data-flx="channel.direct-message.add-friend-view.add-friend-form" />
				) : (
					<>
						<UserCirclePlusIcon
							weight="fill"
							className={styles.heroIcon}
							data-flx="channel.direct-message.add-friend-view.hero-icon"
						/>
						<h2 className={styles.title} data-flx="channel.direct-message.add-friend-view.title">
							{i18n._(ADD_FRIEND_DESCRIPTOR)}
						</h2>
						<p className={styles.subtitle} data-flx="channel.direct-message.add-friend-view.subtitle">
							<Trans>You can add friends with their username.</Trans>
						</p>
						<div className={styles.formContainer} data-flx="channel.direct-message.add-friend-view.form-container">
							<AddFriendForm data-flx="channel.direct-message.add-friend-view.add-friend-form" />
						</div>
					</>
				)}
			</div>
		</div>
	);
});
