// SPDX-License-Identifier: AGPL-3.0-or-later

import {useAccountAvatarURL} from '@app/features/auth/AccountAvatarUtils';
import Accounts from '@app/features/auth/state/Accounts';
import * as ChannelUtils from '@app/features/channel/utils/ChannelUtils';
import type {Account} from '@app/features/platform/state/AuthSession';
import styles from '@app/features/search/components/quick_switcher/OtherAccountAvatar.module.css';
import {MockAvatar} from '@app/features/ui/components/MockAvatar';
import {ChannelTypes} from '@fluxer/constants/src/ChannelConstants';
import type {MediaProxyImageSize} from '@fluxer/constants/src/MediaProxyImageSizes';
import {AtIcon, UsersIcon} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import {observer} from 'mobx-react-lite';

type OtherAccountAvatarSize = 16 | 24 | 32;

const IMAGE_SIZE: Record<OtherAccountAvatarSize, MediaProxyImageSize> = {16: 32, 24: 48, 32: 64};

const AccountAvatarImage = ({account, size}: {account: Account; size: OtherAccountAvatarSize}) => {
	const avatarUrl = useAccountAvatarURL(account, IMAGE_SIZE[size]);
	return (
		<MockAvatar
			size={size}
			avatarUrl={avatarUrl}
			userTag={account.userData?.username ?? account.userId}
			data-flx="search.other-account-avatar.mock-avatar"
		/>
	);
};

export const OtherAccountAvatar = observer(({accountKey, size}: {accountKey: string; size: OtherAccountAvatarSize}) => {
	const account = Accounts.getAccount(accountKey);
	if (account == null) {
		return <MockAvatar size={size} data-flx="search.other-account-avatar.fallback" />;
	}
	return <AccountAvatarImage account={account} size={size} data-flx="search.other-account-avatar.image" />;
});

function channelTypeIcon(channelType: number) {
	if (channelType === ChannelTypes.DM) {
		return <AtIcon weight="bold" className={styles.badgeIcon} data-flx="search.other-account-channel-icon.at-icon" />;
	}
	if (channelType === ChannelTypes.GROUP_DM) {
		return (
			<UsersIcon weight="bold" className={styles.badgeIcon} data-flx="search.other-account-channel-icon.users-icon" />
		);
	}
	return ChannelUtils.getIcon({type: channelType}, {className: styles.badgeIcon, weight: 'bold'});
}

export const OtherAccountChannelIcon = ({
	accountKey,
	channelType,
	highlighted,
}: {
	accountKey: string;
	channelType: number;
	highlighted: boolean;
}) => (
	<div className={styles.frame} data-flx="search.other-account-channel-icon.frame">
		<div className={styles.cutout} data-flx="search.other-account-channel-icon.cutout">
			<OtherAccountAvatar accountKey={accountKey} size={24} data-flx="search.other-account-channel-icon.avatar" />
		</div>
		<div
			className={clsx(styles.badge, highlighted && styles.badgeHighlighted)}
			data-flx="search.other-account-channel-icon.badge"
		>
			{channelTypeIcon(channelType)}
		</div>
	</div>
);
