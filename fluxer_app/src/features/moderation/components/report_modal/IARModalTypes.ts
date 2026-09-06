// SPDX-License-Identifier: AGPL-3.0-or-later

import type {Channel} from '@app/features/channel/models/Channel';
import type {Message} from '@app/features/messaging/models/MessagingMessage';
import type {ButtonProps} from '@app/features/ui/button/Button';
import type {RadioOption} from '@app/features/ui/radio_group/RadioGroup';
import type {User} from '@app/features/user/models/User';

export interface IARActionCardConfig {
	id: string;
	title: string;
	description: string;
	label: string;
	buttonVariant?: ButtonProps['variant'];
	onClick: () => void;
	disabled?: boolean;
	disabledTooltip?: string;
}

export interface IARActionHandlers {
	onBlockUser: () => void;
	onCloseDM: () => void;
	onCopyMessageLink: () => void;
	onLeaveCommunity: () => void;
	onOpenCommunicationSettings: () => void;
	onOpenConnectionsSettings: () => void;
	onDeleteMessage: () => void;
	onBanUser: () => void;
}

export interface IARCopyBlock {
	title: string;
	body: string;
}

export type IARRadioOption<T> = RadioOption<T> & {
	name: string;
	desc?: string;
};
export type IARStep = 'category' | 'reason' | 'success';
export type IARContext =
	| {
			type: 'message';
			message: Message;
	  }
	| {
			type: 'user';
			user: User;
			guildId?: string;
	  }
	| {
			type: 'guild';
			guild: {id: string; name: string};
			inviteCode?: string;
	  };

export interface IARResolvedContext {
	title: string;
	currentChannel: Channel | null;
	reportedUser: User | null;
	isReportedUserBlocked: boolean;
	leaveableGuildId: string | null;
	hasCommunityContext: boolean;
	dmChannel: Channel | null;
	dmDisplayName: string;
	isFocusedOnDMWithUser: boolean;
	isLeaveableGuildOwner: boolean;
	canDeleteReportedMessage: boolean;
	banGuildId: string | null;
	canBanReportedUser: boolean;
}
