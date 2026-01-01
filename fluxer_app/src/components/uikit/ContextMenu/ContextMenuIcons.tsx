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

import {
	ArrowBendUpLeftIcon,
	ArrowBendUpRightIcon,
	ArrowSquareOutIcon,
	BellIcon,
	BellSlashIcon,
	BookmarkSimpleIcon,
	BugBeetleIcon,
	ChatCircleDotsIcon,
	CheckCircleIcon,
	CheckIcon,
	ClipboardTextIcon,
	ClockCounterClockwiseIcon,
	CopyIcon as CopyIconPhosphor,
	DownloadSimpleIcon,
	FlagIcon,
	FolderPlusIcon,
	GearIcon,
	GlobeIcon,
	ImageSquareIcon,
	LinkIcon,
	NotePencilIcon,
	PencilIcon,
	PhoneIcon,
	PlusCircleIcon,
	ProhibitIcon,
	PushPinIcon,
	ShieldIcon,
	SignOutIcon,
	SmileyIcon,
	SmileyXEyesIcon,
	SnowflakeIcon,
	SpeakerHighIcon,
	StarIcon,
	TrashIcon,
	UserCircleIcon,
	UserMinusIcon,
	UserPlusIcon,
	VideoCameraIcon,
	WrenchIcon,
	XIcon,
} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import type React from 'react';

interface IconProps {
	size?: number;
}

export const ReplyIcon: React.FC<IconProps> = observer(({size = 16}) => (
	<ArrowBendUpLeftIcon size={size} weight="fill" />
));
export const ForwardIcon: React.FC<IconProps> = observer(({size = 16}) => (
	<ArrowBendUpRightIcon size={size} weight="fill" />
));
export const EditIcon: React.FC<IconProps> = observer(({size = 16}) => <PencilIcon size={size} weight="fill" />);
export const DeleteIcon: React.FC<IconProps> = observer(({size = 16}) => <TrashIcon size={size} weight="fill" />);
export const AddReactionIcon: React.FC<IconProps> = observer(({size = 16}) => <SmileyIcon size={size} weight="fill" />);
export const RemoveAllReactionsIcon: React.FC<IconProps> = observer(({size = 16}) => (
	<SmileyXEyesIcon size={size} weight="fill" />
));
export const PinIcon: React.FC<IconProps> = observer(({size = 16}) => <PushPinIcon size={size} weight="fill" />);

export const BookmarkIcon: React.FC<IconProps & {filled?: boolean}> = observer(({size = 16, filled = false}) => (
	<BookmarkSimpleIcon size={size} weight={filled ? 'fill' : 'regular'} />
));

export const FavoriteIcon: React.FC<IconProps & {filled?: boolean}> = observer(({size = 16, filled = false}) => (
	<StarIcon size={size} weight={filled ? 'fill' : 'regular'} />
));

export const CopyTextIcon: React.FC<IconProps> = observer(({size = 16}) => (
	<ClipboardTextIcon size={size} weight="fill" />
));
export const CopyLinkIcon: React.FC<IconProps> = observer(({size = 16}) => <LinkIcon size={size} weight="bold" />);
export const CopyIdIcon: React.FC<IconProps> = observer(({size = 16}) => (
	<SnowflakeIcon size={size} weight="regular" />
));
export const CopyIcon: React.FC<IconProps> = observer(({size = 16}) => <CopyIconPhosphor size={size} weight="fill" />);
export const CopyUserIdIcon: React.FC<IconProps> = observer(({size = 16}) => (
	<SnowflakeIcon size={size} weight="regular" />
));
export const CopyFluxerTagIcon: React.FC<IconProps> = observer(({size = 16}) => (
	<CopyIconPhosphor size={size} weight="fill" />
));

export const OpenLinkIcon: React.FC<IconProps> = observer(({size = 16}) => (
	<ArrowSquareOutIcon size={size} weight="regular" />
));

export const SaveIcon: React.FC<IconProps> = observer(({size = 16}) => (
	<DownloadSimpleIcon size={size} weight="fill" />
));
export const SuppressEmbedsIcon: React.FC<IconProps> = observer(({size = 16}) => (
	<ImageSquareIcon size={size} weight="fill" />
));

export const MarkAsReadIcon: React.FC<IconProps> = observer(({size = 16}) => (
	<CheckIcon size={size} weight="regular" />
));
export const MarkAsUnreadIcon: React.FC<IconProps> = observer(({size = 16}) => (
	<ChatCircleDotsIcon size={size} weight="fill" />
));
export const MuteIcon: React.FC<IconProps> = observer(({size = 16}) => <BellSlashIcon size={size} weight="fill" />);
export const NotificationSettingsIcon: React.FC<IconProps> = observer(({size = 16}) => (
	<BellIcon size={size} weight="fill" />
));

export const InviteIcon: React.FC<IconProps> = observer(({size = 16}) => <UserPlusIcon size={size} weight="fill" />);
export const CreateChannelIcon: React.FC<IconProps> = observer(({size = 16}) => (
	<PlusCircleIcon size={size} weight="fill" />
));
export const CreateCategoryIcon: React.FC<IconProps> = observer(({size = 16}) => (
	<FolderPlusIcon size={size} weight="fill" />
));
export const SettingsIcon: React.FC<IconProps> = observer(({size = 16}) => <GearIcon size={size} weight="fill" />);
export const PrivacySettingsIcon: React.FC<IconProps> = observer(({size = 16}) => (
	<ShieldIcon size={size} weight="fill" />
));
export const LeaveIcon: React.FC<IconProps> = observer(({size = 16}) => <SignOutIcon size={size} weight="fill" />);
export const EditProfileIcon: React.FC<IconProps> = observer(({size = 16}) => (
	<UserCircleIcon size={size} weight="fill" />
));

export const VoiceCallIcon: React.FC<IconProps> = observer(({size = 16}) => <PhoneIcon size={size} weight="fill" />);
export const VideoCallIcon: React.FC<IconProps> = observer(({size = 16}) => (
	<VideoCameraIcon size={size} weight="fill" />
));

export const DebugIcon: React.FC<IconProps> = observer(({size = 16}) => <BugBeetleIcon size={size} weight="fill" />);

export const AddNoteIcon: React.FC<IconProps> = observer(({size = 16}) => <NotePencilIcon size={size} weight="fill" />);

export const SendFriendRequestIcon: React.FC<IconProps> = observer(({size = 16}) => (
	<UserPlusIcon size={size} weight="fill" />
));
export const AcceptFriendRequestIcon: React.FC<IconProps> = observer(({size = 16}) => (
	<CheckCircleIcon size={size} weight="fill" />
));
export const RemoveFriendIcon: React.FC<IconProps> = observer(({size = 16}) => (
	<UserMinusIcon size={size} weight="fill" />
));
export const IgnoreFriendRequestIcon: React.FC<IconProps> = observer(({size = 16}) => (
	<XIcon size={size} weight="fill" />
));
export const CancelFriendRequestIcon: React.FC<IconProps> = observer(({size = 16}) => (
	<ClockCounterClockwiseIcon size={size} weight="fill" />
));
export const BlockUserIcon: React.FC<IconProps> = observer(({size = 16}) => <ProhibitIcon size={size} weight="fill" />);
export const ReportUserIcon: React.FC<IconProps> = observer(({size = 16}) => <FlagIcon size={size} weight="fill" />);
export const ViewGlobalProfileIcon: React.FC<IconProps> = observer(({size = 16}) => (
	<GlobeIcon size={size} weight="fill" />
));

export const WrenchToolIcon: React.FC<IconProps> = observer(({size = 16}) => <WrenchIcon size={size} weight="fill" />);

export const SpeakIcon: React.FC<IconProps> = observer(({size = 16}) => <SpeakerHighIcon size={size} weight="fill" />);
