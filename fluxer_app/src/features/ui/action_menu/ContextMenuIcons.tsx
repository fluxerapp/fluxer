// SPDX-License-Identifier: AGPL-3.0-or-later

import {remFromPx} from '@app/features/theme/layout/RemFromPx';

import {
	ArrowBendUpLeftIcon,
	ArrowBendUpRightIcon,
	ArrowLeftIcon,
	ArrowRightIcon,
	ArrowSquareOutIcon,
	ArrowsClockwiseIcon,
	ArrowsLeftRightIcon,
	ArrowsOutCardinalIcon,
	AtIcon,
	BellIcon,
	BellSlashIcon,
	BookmarkSimpleIcon,
	BootIcon,
	BugBeetleIcon,
	BugIcon,
	CameraIcon,
	CameraSlashIcon,
	CaretDownIcon,
	CaretRightIcon,
	CaretUpIcon,
	ChatCircleIcon,
	CheckCircleIcon,
	CircleNotchIcon,
	ClipboardTextIcon,
	ClockCounterClockwiseIcon,
	ClockIcon,
	CopySimpleIcon,
	CrownIcon,
	DotsThreeIcon,
	DotsThreeVerticalIcon,
	DownloadSimpleIcon,
	EnvelopeOpenIcon,
	EyeIcon,
	EyeSlashIcon,
	FlagIcon,
	FolderPlusIcon,
	FunnelIcon,
	GavelIcon,
	GearIcon,
	GlobeIcon,
	GridFourIcon,
	type IconWeight,
	LinkBreakIcon,
	LinkIcon,
	MagnifyingGlassIcon,
	MicrophoneIcon,
	MicrophoneSlashIcon,
	MonitorPlayIcon,
	NotePencilIcon,
	PaperPlaneIcon,
	PaperPlaneRightIcon,
	PencilIcon,
	PencilSimpleIcon,
	PhoneIcon,
	PhoneXIcon,
	CheckIcon as PhosphorCheckIcon,
	CopyIcon as PhosphorCopyIcon,
	TranslateIcon as PhosphorTranslateIcon,
	PlusCircleIcon,
	PlusIcon,
	ProhibitIcon,
	PushPinIcon,
	ShieldIcon,
	SignOutIcon,
	SmileyIcon,
	SnowflakeIcon,
	SortAscendingIcon,
	SpeakerHighIcon,
	SpeakerSimpleSlashIcon,
	SpeakerSlashIcon,
	StarIcon,
	StopCircleIcon,
	TicketIcon,
	TrashIcon,
	UserCircleIcon,
	UserIcon,
	UserListIcon,
	UserMinusIcon,
	UserPlusIcon,
	UsersIcon,
	VideoCameraIcon,
	VideoCameraSlashIcon,
	VideoIcon,
	WrenchIcon,
	XIcon,
} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import type React from 'react';

interface IconProps {
	size?: number;
	weight?: IconWeight;
	className?: string;
}

export const ReplyIcon: React.FC<IconProps> = observer(({size = 16}) => (
	<ArrowBendUpLeftIcon
		size={remFromPx(size)}
		weight="fill"
		data-flx="ui.action-menu.context-menu-icons.reply-icon.arrow-bend-up-left-icon"
	/>
));
export const ForwardIcon: React.FC<IconProps> = observer(({size = 16}) => (
	<ArrowBendUpRightIcon
		size={remFromPx(size)}
		weight="fill"
		data-flx="ui.action-menu.context-menu-icons.forward-icon.arrow-bend-up-right-icon"
	/>
));
export const EditIcon: React.FC<IconProps> = observer(({size = 16, weight = 'fill'}) => (
	<PencilIcon
		size={remFromPx(size)}
		weight={weight}
		data-flx="ui.action-menu.context-menu-icons.edit-icon.pencil-icon"
	/>
));
export const DeleteIcon: React.FC<IconProps> = observer(({size = 16}) => (
	<TrashIcon size={remFromPx(size)} weight="fill" data-flx="ui.action-menu.context-menu-icons.delete-icon.trash-icon" />
));
export const PinIcon: React.FC<IconProps> = observer(({size = 16}) => (
	<PushPinIcon
		size={remFromPx(size)}
		weight="fill"
		data-flx="ui.action-menu.context-menu-icons.pin-icon.push-pin-icon"
	/>
));
export const AddReactionIcon: React.FC<IconProps> = observer(({size = 16}) => (
	<SmileyIcon
		size={remFromPx(size)}
		weight="fill"
		data-flx="ui.action-menu.context-menu-icons.add-reaction-icon.smiley-icon"
	/>
));
export const RemoveAllReactionsIcon: React.FC<IconProps> = observer(({size = 16}) => (
	<XIcon
		size={remFromPx(size)}
		weight="bold"
		data-flx="ui.action-menu.context-menu-icons.remove-all-reactions-icon.x-icon"
	/>
));
export const BookmarkIcon: React.FC<IconProps & {filled?: boolean}> = observer(({size = 16, filled = false}) => (
	<BookmarkSimpleIcon
		size={remFromPx(size)}
		weight={filled ? 'fill' : 'regular'}
		data-flx="ui.action-menu.context-menu-icons.bookmark-icon.bookmark-simple-icon"
	/>
));
export const FavoriteIcon: React.FC<IconProps & {filled?: boolean}> = observer(({size = 16, filled = false}) => (
	<StarIcon
		size={remFromPx(size)}
		weight={filled ? 'fill' : 'regular'}
		data-flx="ui.action-menu.context-menu-icons.favorite-icon.star-icon"
	/>
));
export const CopyTextIcon: React.FC<IconProps> = observer(({size = 16}) => (
	<ClipboardTextIcon
		size={remFromPx(size)}
		weight="fill"
		data-flx="ui.action-menu.context-menu-icons.copy-text-icon.clipboard-text-icon"
	/>
));
export const CopyLinkIcon: React.FC<IconProps> = observer(({size = 16}) => (
	<LinkIcon
		size={remFromPx(size)}
		weight="bold"
		data-flx="ui.action-menu.context-menu-icons.copy-link-icon.link-icon"
	/>
));
export const CopyIdIcon: React.FC<IconProps> = observer(({size = 16}) => (
	<SnowflakeIcon
		size={remFromPx(size)}
		weight="regular"
		data-flx="ui.action-menu.context-menu-icons.copy-id-icon.snowflake-icon"
	/>
));
export const CopyIcon: React.FC<IconProps> = observer(({size = 16}) => (
	<CopySimpleIcon
		size={remFromPx(size)}
		weight="fill"
		data-flx="ui.action-menu.context-menu-icons.copy-icon.copy-simple-icon"
	/>
));
export const OpenLinkIcon: React.FC<IconProps> = observer(({size = 16}) => (
	<ArrowSquareOutIcon
		size={remFromPx(size)}
		weight="regular"
		data-flx="ui.action-menu.context-menu-icons.open-link-icon.arrow-square-out-icon"
	/>
));
export const RetryIcon: React.FC<IconProps> = observer(({size = 16}) => (
	<ArrowsClockwiseIcon
		size={remFromPx(size)}
		weight="fill"
		data-flx="ui.action-menu.context-menu-icons.retry-icon.arrows-clockwise-icon"
	/>
));
export const SaveIcon: React.FC<IconProps> = observer(({size = 16}) => (
	<DownloadSimpleIcon
		size={remFromPx(size)}
		weight="fill"
		data-flx="ui.action-menu.context-menu-icons.save-icon.download-simple-icon"
	/>
));
export const SuppressEmbedsIcon: React.FC<IconProps> = observer(({size = 16}) => (
	<LinkBreakIcon
		size={remFromPx(size)}
		weight="bold"
		data-flx="ui.action-menu.context-menu-icons.suppress-embeds-icon.link-break-icon"
	/>
));
export const MarkAsReadIcon: React.FC<IconProps> = observer(({size = 16}) => (
	<EyeIcon
		size={remFromPx(size)}
		weight="fill"
		data-flx="ui.action-menu.context-menu-icons.mark-as-read-icon.eye-icon"
	/>
));
export const MarkAsUnreadIcon: React.FC<IconProps> = observer(({size = 16}) => (
	<EnvelopeOpenIcon
		size={remFromPx(size)}
		weight="fill"
		data-flx="ui.action-menu.context-menu-icons.mark-as-unread-icon.envelope-open-icon"
	/>
));
export const MuteIcon: React.FC<IconProps> = observer(({size = 16}) => (
	<BellSlashIcon
		size={remFromPx(size)}
		weight="fill"
		data-flx="ui.action-menu.context-menu-icons.mute-icon.bell-slash-icon"
	/>
));
export const NotificationSettingsIcon: React.FC<IconProps> = observer(({size = 16}) => (
	<BellIcon
		size={remFromPx(size)}
		weight="fill"
		data-flx="ui.action-menu.context-menu-icons.notification-settings-icon.bell-icon"
	/>
));
export const InviteIcon: React.FC<IconProps> = observer(({size = 16}) => (
	<UserPlusIcon
		size={remFromPx(size)}
		weight="fill"
		data-flx="ui.action-menu.context-menu-icons.invite-icon.user-plus-icon"
	/>
));
export const CreateChannelIcon: React.FC<IconProps> = observer(({size = 16}) => (
	<PlusCircleIcon
		size={remFromPx(size)}
		weight="fill"
		data-flx="ui.action-menu.context-menu-icons.create-channel-icon.plus-circle-icon"
	/>
));
export const CreateCategoryIcon: React.FC<IconProps> = observer(({size = 16}) => (
	<FolderPlusIcon
		size={remFromPx(size)}
		weight="fill"
		data-flx="ui.action-menu.context-menu-icons.create-category-icon.folder-plus-icon"
	/>
));
export const SettingsIcon: React.FC<IconProps> = observer(({size = 16}) => (
	<GearIcon size={remFromPx(size)} weight="fill" data-flx="ui.action-menu.context-menu-icons.settings-icon.gear-icon" />
));
export const PrivacySettingsIcon: React.FC<IconProps> = observer(({size = 16}) => (
	<ShieldIcon
		size={remFromPx(size)}
		weight="fill"
		data-flx="ui.action-menu.context-menu-icons.privacy-settings-icon.shield-icon"
	/>
));
export const LeaveIcon: React.FC<IconProps> = observer(({size = 16}) => (
	<SignOutIcon
		size={remFromPx(size)}
		weight="fill"
		data-flx="ui.action-menu.context-menu-icons.leave-icon.sign-out-icon"
	/>
));
export const EditProfileIcon: React.FC<IconProps> = observer(({size = 16}) => (
	<UserCircleIcon
		size={remFromPx(size)}
		weight="fill"
		data-flx="ui.action-menu.context-menu-icons.edit-profile-icon.user-circle-icon"
	/>
));
export const ViewGlobalProfileIcon: React.FC<IconProps> = observer(({size = 16}) => (
	<GlobeIcon
		size={remFromPx(size)}
		weight="fill"
		data-flx="ui.action-menu.context-menu-icons.view-global-profile-icon.globe-icon"
	/>
));
export const VoiceCallIcon: React.FC<IconProps> = observer(({size = 16}) => (
	<PhoneIcon
		size={remFromPx(size)}
		weight="fill"
		data-flx="ui.action-menu.context-menu-icons.voice-call-icon.phone-icon"
	/>
));
export const VideoCallIcon: React.FC<IconProps> = observer(({size = 16}) => (
	<VideoCameraIcon
		size={remFromPx(size)}
		weight="fill"
		data-flx="ui.action-menu.context-menu-icons.video-call-icon.video-camera-icon"
	/>
));
export const SpeakIcon: React.FC<IconProps> = observer(({size = 16}) => (
	<SpeakerHighIcon
		size={remFromPx(size)}
		weight="fill"
		data-flx="ui.action-menu.context-menu-icons.speak-icon.speaker-high-icon"
	/>
));
export const SendFriendRequestIcon: React.FC<IconProps> = observer(({size = 16, weight = 'fill'}) => (
	<UserPlusIcon
		size={remFromPx(size)}
		weight={weight}
		data-flx="ui.action-menu.context-menu-icons.send-friend-request-icon.user-plus-icon"
	/>
));
export const AcceptFriendRequestIcon: React.FC<IconProps> = observer(({size = 16}) => (
	<CheckCircleIcon
		size={remFromPx(size)}
		weight="fill"
		data-flx="ui.action-menu.context-menu-icons.accept-friend-request-icon.check-circle-icon"
	/>
));
export const RemoveFriendIcon: React.FC<IconProps> = observer(({size = 16, weight = 'fill'}) => (
	<UserMinusIcon
		size={remFromPx(size)}
		weight={weight}
		data-flx="ui.action-menu.context-menu-icons.remove-friend-icon.user-minus-icon"
	/>
));
export const IgnoreFriendRequestIcon: React.FC<IconProps> = observer(({size = 16, weight = 'bold'}) => (
	<XIcon
		size={remFromPx(size)}
		weight={weight}
		data-flx="ui.action-menu.context-menu-icons.ignore-friend-request-icon.x-icon"
	/>
));
export const CancelFriendRequestIcon: React.FC<IconProps> = observer(({size = 16, weight = 'fill'}) => (
	<ClockCounterClockwiseIcon
		size={remFromPx(size)}
		weight={weight}
		data-flx="ui.action-menu.context-menu-icons.cancel-friend-request-icon.clock-counter-clockwise-icon"
	/>
));
export const BlockUserIcon: React.FC<IconProps> = observer(({size = 16, weight = 'fill'}) => (
	<ProhibitIcon
		size={remFromPx(size)}
		weight={weight}
		data-flx="ui.action-menu.context-menu-icons.block-user-icon.prohibit-icon"
	/>
));
export const ReportUserIcon: React.FC<IconProps> = observer(({size = 16, weight = 'fill'}) => (
	<FlagIcon
		size={remFromPx(size)}
		weight={weight}
		data-flx="ui.action-menu.context-menu-icons.report-user-icon.flag-icon"
	/>
));
export const AddNoteIcon: React.FC<IconProps> = observer(({size = 16}) => (
	<NotePencilIcon
		size={remFromPx(size)}
		weight="fill"
		data-flx="ui.action-menu.context-menu-icons.add-note-icon.note-pencil-icon"
	/>
));
export const DebugIcon: React.FC<IconProps> = observer(({size = 16}) => (
	<BugBeetleIcon
		size={remFromPx(size)}
		weight="fill"
		data-flx="ui.action-menu.context-menu-icons.debug-icon.bug-beetle-icon"
	/>
));
export const WrenchToolIcon: React.FC<IconProps> = observer(({size = 16}) => (
	<WrenchIcon
		size={remFromPx(size)}
		weight="fill"
		data-flx="ui.action-menu.context-menu-icons.wrench-tool-icon.wrench-icon"
	/>
));
export const EditMessageIcon: React.FC<IconProps> = observer(({size = 16}) => (
	<PencilSimpleIcon
		size={remFromPx(size)}
		weight="fill"
		data-flx="ui.action-menu.context-menu-icons.edit-message-icon.pencil-simple-icon"
	/>
));
export const CopyMessageTextIcon: React.FC<IconProps> = observer(({size = 16}) => (
	<PhosphorCopyIcon
		size={remFromPx(size)}
		weight="fill"
		data-flx="ui.action-menu.context-menu-icons.copy-message-text-icon.phosphor-copy-icon"
	/>
));
export const DebugMessageIcon: React.FC<IconProps> = observer(({size = 16}) => (
	<BugIcon
		size={remFromPx(size)}
		weight="fill"
		data-flx="ui.action-menu.context-menu-icons.debug-message-icon.bug-icon"
	/>
));
export const SpeakMessageIcon: React.FC<IconProps> = observer(({size = 16}) => (
	<SpeakerHighIcon
		size={remFromPx(size)}
		weight="fill"
		data-flx="ui.action-menu.context-menu-icons.speak-message-icon.speaker-high-icon"
	/>
));
export const StopSpeakingIcon: React.FC<IconProps> = observer(({size = 16}) => (
	<StopCircleIcon
		size={remFromPx(size)}
		weight="fill"
		data-flx="ui.action-menu.context-menu-icons.stop-speaking-icon.stop-circle-icon"
	/>
));
export const MoreIcon: React.FC<IconProps> = observer(({size = 16}) => (
	<DotsThreeIcon
		size={remFromPx(size)}
		weight="bold"
		data-flx="ui.action-menu.context-menu-icons.more-icon.dots-three-icon"
	/>
));
export const ViewReactionsIcon: React.FC<IconProps> = observer(({size = 16}) => (
	<SmileyIcon
		size={remFromPx(size)}
		weight="fill"
		data-flx="ui.action-menu.context-menu-icons.view-reactions-icon.smiley-icon"
	/>
));
export const ReportMessageIcon: React.FC<IconProps> = observer(({size = 16}) => (
	<FlagIcon
		size={remFromPx(size)}
		weight="fill"
		data-flx="ui.action-menu.context-menu-icons.report-message-icon.flag-icon"
	/>
));
export const EditSimpleIcon: React.FC<IconProps> = observer(({size = 16}) => (
	<PencilSimpleIcon
		size={remFromPx(size)}
		weight="fill"
		data-flx="ui.action-menu.context-menu-icons.edit-simple-icon.pencil-simple-icon"
	/>
));
export const ExpandIcon: React.FC<IconProps> = observer(({size = 16}) => (
	<CaretDownIcon
		size={remFromPx(size)}
		weight="bold"
		data-flx="ui.action-menu.context-menu-icons.expand-icon.caret-down-icon"
	/>
));
export const CollapseIcon: React.FC<IconProps> = observer(({size = 16}) => (
	<CaretUpIcon
		size={remFromPx(size)}
		weight="bold"
		data-flx="ui.action-menu.context-menu-icons.collapse-icon.caret-up-icon"
	/>
));
export const TransferOwnershipIcon: React.FC<IconProps> = observer(({size = 16}) => (
	<CrownIcon
		size={remFromPx(size)}
		weight="fill"
		data-flx="ui.action-menu.context-menu-icons.transfer-ownership-icon.crown-icon"
	/>
));
export const RemoveFromGroupIcon: React.FC<IconProps> = observer(({size = 16}) => (
	<UserMinusIcon
		size={remFromPx(size)}
		weight="fill"
		data-flx="ui.action-menu.context-menu-icons.remove-from-group-icon.user-minus-icon"
	/>
));
export const CloseDMIcon: React.FC<IconProps> = observer(({size = 16}) => (
	<XIcon size={remFromPx(size)} weight="bold" data-flx="ui.action-menu.context-menu-icons.close-dm-icon.x-icon" />
));
export const ChangeNicknameIcon: React.FC<IconProps> = observer(({size = 16}) => (
	<PencilSimpleIcon
		size={remFromPx(size)}
		weight="fill"
		data-flx="ui.action-menu.context-menu-icons.change-nickname-icon.pencil-simple-icon"
	/>
));
export const CreateIcon: React.FC<IconProps> = observer(({size = 16}) => (
	<PlusIcon size={remFromPx(size)} weight="bold" data-flx="ui.action-menu.context-menu-icons.create-icon.plus-icon" />
));
export const HideIcon: React.FC<IconProps> = observer(({size = 16}) => (
	<EyeSlashIcon
		size={remFromPx(size)}
		weight="fill"
		data-flx="ui.action-menu.context-menu-icons.hide-icon.eye-slash-icon"
	/>
));
export const MoveToIcon: React.FC<IconProps> = observer(({size = 16}) => (
	<ArrowsOutCardinalIcon
		size={remFromPx(size)}
		weight="fill"
		data-flx="ui.action-menu.context-menu-icons.move-to-icon.arrows-out-cardinal-icon"
	/>
));
export const ViewDetailsIcon: React.FC<IconProps> = observer(({size = 16}) => (
	<EyeIcon
		size={remFromPx(size)}
		weight="bold"
		data-flx="ui.action-menu.context-menu-icons.view-details-icon.eye-icon"
	/>
));
export const RevokeBanIcon: React.FC<IconProps> = observer(({size = 16}) => (
	<ProhibitIcon
		size={remFromPx(size)}
		weight="bold"
		data-flx="ui.action-menu.context-menu-icons.revoke-ban-icon.prohibit-icon"
	/>
));
export const RemoveFromFavoritesIcon: React.FC<IconProps> = observer(({size = 16}) => (
	<StarIcon
		size={remFromPx(size)}
		weight="fill"
		data-flx="ui.action-menu.context-menu-icons.remove-from-favorites-icon.star-icon"
	/>
));
export const OpenInCommunityIcon: React.FC<IconProps> = observer(({size = 16}) => (
	<ArrowSquareOutIcon
		size={remFromPx(size)}
		weight="fill"
		data-flx="ui.action-menu.context-menu-icons.open-in-community-icon.arrow-square-out-icon"
	/>
));
export const PopOutIcon: React.FC<IconProps> = observer(({size = 16}) => (
	<ArrowSquareOutIcon
		size={remFromPx(size)}
		weight="fill"
		data-flx="ui.action-menu.context-menu-icons.pop-out-icon.arrow-square-out-icon"
	/>
));
export const CheckIcon: React.FC<IconProps> = observer(({size = 16, weight = 'bold', className}) => (
	<PhosphorCheckIcon
		size={remFromPx(size)}
		weight={weight}
		className={className}
		data-flx="ui.action-menu.context-menu-icons.check-icon.phosphor-check-icon"
	/>
));
export const PreviousIcon: React.FC<IconProps> = observer(({size = 16, className}) => (
	<ArrowLeftIcon
		size={remFromPx(size)}
		weight="fill"
		className={className}
		data-flx="ui.action-menu.context-menu-icons.previous-icon.arrow-left-icon"
	/>
));
export const NextIcon: React.FC<IconProps> = observer(({size = 16, className}) => (
	<ArrowRightIcon
		size={remFromPx(size)}
		weight="fill"
		className={className}
		data-flx="ui.action-menu.context-menu-icons.next-icon.arrow-right-icon"
	/>
));
export const ExpandChevronIcon: React.FC<IconProps> = observer(({size = 16, weight = 'bold', className}) => (
	<CaretDownIcon
		size={remFromPx(size)}
		weight={weight}
		className={className}
		data-flx="ui.action-menu.context-menu-icons.expand-chevron-icon.caret-down-icon"
	/>
));
export const CollapseChevronIcon: React.FC<IconProps> = observer(({size = 16, weight = 'bold', className}) => (
	<CaretUpIcon
		size={remFromPx(size)}
		weight={weight}
		className={className}
		data-flx="ui.action-menu.context-menu-icons.collapse-chevron-icon.caret-up-icon"
	/>
));
export const ChevronRightIcon: React.FC<IconProps> = observer(({size = 16, weight = 'bold', className}) => (
	<CaretRightIcon
		size={remFromPx(size)}
		weight={weight}
		className={className}
		data-flx="ui.action-menu.context-menu-icons.chevron-right-icon.caret-right-icon"
	/>
));
export const LoadingIcon: React.FC<IconProps> = observer(({size = 16, className}) => (
	<CircleNotchIcon
		size={remFromPx(size)}
		className={className}
		data-flx="ui.action-menu.context-menu-icons.loading-icon.circle-notch-icon"
	/>
));
export const FilterIcon: React.FC<IconProps> = observer(({size = 16, weight = 'bold', className}) => (
	<FunnelIcon
		size={remFromPx(size)}
		weight={weight}
		className={className}
		data-flx="ui.action-menu.context-menu-icons.filter-icon.funnel-icon"
	/>
));
export const SearchIcon: React.FC<IconProps> = observer(({size = 16, weight = 'bold', className}) => (
	<MagnifyingGlassIcon
		size={remFromPx(size)}
		weight={weight}
		className={className}
		data-flx="ui.action-menu.context-menu-icons.search-icon.magnifying-glass-icon"
	/>
));
export const TranslateIcon: React.FC<IconProps> = observer(({size = 16, weight = 'bold', className}) => (
	<PhosphorTranslateIcon
		size={remFromPx(size)}
		weight={weight}
		className={className}
		data-flx="ui.action-menu.context-menu-icons.translate-icon.phosphor-translate-icon"
	/>
));
export const SortIcon: React.FC<IconProps> = observer(({size = 16, weight = 'bold', className}) => (
	<SortAscendingIcon
		size={remFromPx(size)}
		weight={weight}
		className={className}
		data-flx="ui.action-menu.context-menu-icons.sort-icon.sort-ascending-icon"
	/>
));
export const UserFilterIcon: React.FC<IconProps> = observer(({size = 16, weight = 'bold', className}) => (
	<UserIcon
		size={remFromPx(size)}
		weight={weight}
		className={className}
		data-flx="ui.action-menu.context-menu-icons.user-filter-icon.user-icon"
	/>
));
export const CloseIcon: React.FC<IconProps> = observer(({size = 16, weight = 'bold', className}) => (
	<XIcon
		size={remFromPx(size)}
		weight={weight}
		className={className}
		data-flx="ui.action-menu.context-menu-icons.close-icon.x-icon"
	/>
));
export const CameraOnIcon: React.FC<IconProps> = observer(({size = 16, className}) => (
	<CameraIcon
		size={remFromPx(size)}
		weight="fill"
		className={className}
		data-flx="ui.action-menu.context-menu-icons.camera-on-icon.camera-icon"
	/>
));
export const CameraOffIcon: React.FC<IconProps> = observer(({size = 16, className}) => (
	<CameraSlashIcon
		size={remFromPx(size)}
		weight="fill"
		className={className}
		data-flx="ui.action-menu.context-menu-icons.camera-off-icon.camera-slash-icon"
	/>
));
export const MicrophoneOnIcon: React.FC<IconProps> = observer(({size = 16, className}) => (
	<MicrophoneIcon
		size={remFromPx(size)}
		weight="fill"
		className={className}
		data-flx="ui.action-menu.context-menu-icons.microphone-on-icon.microphone-icon"
	/>
));
export const MicrophoneOffIcon: React.FC<IconProps> = observer(({size = 16, className}) => (
	<MicrophoneSlashIcon
		size={remFromPx(size)}
		weight="fill"
		className={className}
		data-flx="ui.action-menu.context-menu-icons.microphone-off-icon.microphone-slash-icon"
	/>
));
export const DisconnectCallIcon: React.FC<IconProps> = observer(({size = 16, className}) => (
	<PhoneXIcon
		size={remFromPx(size)}
		weight="fill"
		className={className}
		data-flx="ui.action-menu.context-menu-icons.disconnect-call-icon.phone-x-icon"
	/>
));
export const DeafenIcon: React.FC<IconProps> = observer(({size = 16, className}) => (
	<SpeakerSlashIcon
		size={remFromPx(size)}
		weight="fill"
		className={className}
		data-flx="ui.action-menu.context-menu-icons.deafen-icon.speaker-slash-icon"
	/>
));
export const UndeafenIcon: React.FC<IconProps> = observer(({size = 16, className}) => (
	<SpeakerHighIcon
		size={remFromPx(size)}
		weight="fill"
		className={className}
		data-flx="ui.action-menu.context-menu-icons.undeafen-icon.speaker-high-icon"
	/>
));
export const NewGroupIcon: React.FC<IconProps> = observer(({size = 16, className}) => (
	<ChatCircleIcon
		size={remFromPx(size)}
		weight="fill"
		className={className}
		data-flx="ui.action-menu.context-menu-icons.new-group-icon.chat-circle-icon"
	/>
));
export const OwnerCrownIcon: React.FC<IconProps> = observer(({size = 16, className}) => (
	<CrownIcon
		size={remFromPx(size)}
		weight="fill"
		className={className}
		data-flx="ui.action-menu.context-menu-icons.owner-crown-icon.crown-icon"
	/>
));
export const MoreOptionsVerticalIcon: React.FC<IconProps> = observer(({size = 16, weight = 'bold', className}) => (
	<DotsThreeVerticalIcon
		size={remFromPx(size)}
		weight={weight}
		className={className}
		data-flx="ui.action-menu.context-menu-icons.more-options-vertical-icon.dots-three-vertical-icon"
	/>
));
export const InvitesIcon: React.FC<IconProps> = observer(({size = 16, className}) => (
	<TicketIcon
		size={remFromPx(size)}
		weight="fill"
		className={className}
		data-flx="ui.action-menu.context-menu-icons.invites-icon.ticket-icon"
	/>
));
export const MembersIcon: React.FC<IconProps> = observer(({size = 16, className}) => (
	<UsersIcon
		size={remFromPx(size)}
		weight="fill"
		className={className}
		data-flx="ui.action-menu.context-menu-icons.members-icon.users-icon"
	/>
));
export const GridViewIcon: React.FC<IconProps> = observer(({size = 16, className}) => (
	<GridFourIcon
		size={remFromPx(size)}
		weight="fill"
		className={className}
		data-flx="ui.action-menu.context-menu-icons.grid-view-icon.grid-four-icon"
	/>
));
export const EchoCancellationIcon: React.FC<IconProps> = observer(({size = 16, className}) => (
	<SpeakerSimpleSlashIcon
		size={remFromPx(size)}
		weight="fill"
		className={className}
		data-flx="ui.action-menu.context-menu-icons.echo-cancellation-icon.speaker-simple-slash-icon"
	/>
));
export const VideoSettingsIcon: React.FC<IconProps> = observer(({size = 16, className}) => (
	<VideoIcon
		size={remFromPx(size)}
		weight="fill"
		className={className}
		data-flx="ui.action-menu.context-menu-icons.video-settings-icon.video-icon"
	/>
));
export const InputDeviceIcon: React.FC<IconProps> = observer(({size = 16, className}) => (
	<MicrophoneIcon
		size={remFromPx(size)}
		weight="fill"
		className={className}
		data-flx="ui.action-menu.context-menu-icons.input-device-icon.microphone-icon"
	/>
));
export const OutputDeviceIcon: React.FC<IconProps> = observer(({size = 16, className}) => (
	<SpeakerHighIcon
		size={remFromPx(size)}
		weight="fill"
		className={className}
		data-flx="ui.action-menu.context-menu-icons.output-device-icon.speaker-high-icon"
	/>
));
export const MentionUserIcon: React.FC<IconProps> = observer(({size = 16}) => (
	<AtIcon size={remFromPx(size)} weight="bold" data-flx="ui.action-menu.context-menu-icons.mention-user-icon.at-icon" />
));
export const ViewProfileIcon: React.FC<IconProps> = observer(({size = 16}) => (
	<UserIcon
		size={remFromPx(size)}
		weight="fill"
		data-flx="ui.action-menu.context-menu-icons.view-profile-icon.user-icon"
	/>
));
export const GroupInvitesIcon: React.FC<IconProps> = observer(({size = 16}) => (
	<TicketIcon
		size={remFromPx(size)}
		weight="fill"
		data-flx="ui.action-menu.context-menu-icons.group-invites-icon.ticket-icon"
	/>
));
export const EditGroupIcon: React.FC<IconProps> = observer(({size = 16}) => (
	<NotePencilIcon
		size={remFromPx(size)}
		weight="fill"
		data-flx="ui.action-menu.context-menu-icons.edit-group-icon.note-pencil-icon"
	/>
));
export const InviteToCommunityIcon: React.FC<IconProps> = observer(({size = 16}) => (
	<UsersIcon
		size={remFromPx(size)}
		weight="fill"
		data-flx="ui.action-menu.context-menu-icons.invite-to-community-icon.users-icon"
	/>
));
export const CollapseCategoryIcon: React.FC<IconProps & {collapsed?: boolean}> = observer(
	({size = 16, collapsed = false}) => (
		<CaretDownIcon
			size={remFromPx(size)}
			weight="bold"
			style={{transform: collapsed ? 'rotate(-90deg)' : undefined}}
			data-flx="ui.action-menu.context-menu-icons.collapse-category-icon.caret-down-icon"
		/>
	),
);
export const MessageUserIcon: React.FC<IconProps> = observer(({size = 16}) => (
	<ChatCircleIcon
		size={remFromPx(size)}
		weight="fill"
		data-flx="ui.action-menu.context-menu-icons.message-user-icon.chat-circle-icon"
	/>
));
export const RingIcon: React.FC<IconProps> = observer(({size = 16}) => (
	<PhoneIcon size={remFromPx(size)} weight="fill" data-flx="ui.action-menu.context-menu-icons.ring-icon.phone-icon" />
));
export const StopRingingIcon: React.FC<IconProps> = observer(({size = 16}) => (
	<PhoneXIcon
		size={remFromPx(size)}
		weight="fill"
		data-flx="ui.action-menu.context-menu-icons.stop-ringing-icon.phone-x-icon"
	/>
));
export const MoveToChannelIcon: React.FC<IconProps> = observer(({size = 16}) => (
	<ArrowsLeftRightIcon
		size={remFromPx(size)}
		weight="fill"
		data-flx="ui.action-menu.context-menu-icons.move-to-channel-icon.arrows-left-right-icon"
	/>
));
export const KickMemberIcon: React.FC<IconProps> = observer(({size = 16}) => (
	<BootIcon
		size={remFromPx(size)}
		weight="fill"
		data-flx="ui.action-menu.context-menu-icons.kick-member-icon.boot-icon"
	/>
));
export const BanMemberIcon: React.FC<IconProps> = observer(({size = 16}) => (
	<GavelIcon
		size={remFromPx(size)}
		weight="fill"
		data-flx="ui.action-menu.context-menu-icons.ban-member-icon.gavel-icon"
	/>
));
export const ManageRolesIcon: React.FC<IconProps> = observer(({size = 16}) => (
	<UserListIcon
		size={remFromPx(size)}
		weight="fill"
		data-flx="ui.action-menu.context-menu-icons.manage-roles-icon.user-list-icon"
	/>
));
export const TimeoutIcon: React.FC<IconProps> = observer(({size = 16}) => (
	<ClockIcon
		size={remFromPx(size)}
		weight="fill"
		data-flx="ui.action-menu.context-menu-icons.timeout-icon.clock-icon"
	/>
));
export const TurnOffCameraIcon: React.FC<IconProps> = observer(({size = 16}) => (
	<VideoCameraSlashIcon
		size={remFromPx(size)}
		weight="fill"
		data-flx="ui.action-menu.context-menu-icons.turn-off-camera-icon.video-camera-slash-icon"
	/>
));
export const TurnOffStreamIcon: React.FC<IconProps> = observer(({size = 16}) => (
	<MonitorPlayIcon
		size={remFromPx(size)}
		weight="fill"
		data-flx="ui.action-menu.context-menu-icons.turn-off-stream-icon.monitor-play-icon"
	/>
));
export const DisconnectIcon: React.FC<IconProps> = observer(({size = 16}) => (
	<PhoneXIcon
		size={remFromPx(size)}
		weight="fill"
		data-flx="ui.action-menu.context-menu-icons.disconnect-icon.phone-x-icon"
	/>
));
export const SelfMuteIcon: React.FC<IconProps> = observer(({size = 16}) => (
	<MicrophoneSlashIcon
		size={remFromPx(size)}
		weight="fill"
		data-flx="ui.action-menu.context-menu-icons.self-mute-icon.microphone-slash-icon"
	/>
));
export const SelfDeafenIcon: React.FC<IconProps> = observer(({size = 16}) => (
	<SpeakerSlashIcon
		size={remFromPx(size)}
		weight="fill"
		data-flx="ui.action-menu.context-menu-icons.self-deafen-icon.speaker-slash-icon"
	/>
));
export const FocusIcon: React.FC<IconProps> = observer(({size = 16}) => (
	<EyeIcon size={remFromPx(size)} weight="fill" data-flx="ui.action-menu.context-menu-icons.focus-icon.eye-icon" />
));
export const UnfocusIcon: React.FC<IconProps> = observer(({size = 16}) => (
	<EyeSlashIcon
		size={remFromPx(size)}
		weight="fill"
		data-flx="ui.action-menu.context-menu-icons.unfocus-icon.eye-slash-icon"
	/>
));
export const SendInvitesIcon: React.FC<IconProps> = observer(({size = 16}) => (
	<PaperPlaneIcon
		size={remFromPx(size)}
		weight="fill"
		data-flx="ui.action-menu.context-menu-icons.send-invites-icon.paper-plane-icon"
	/>
));
export const SendInviteToCommunityIcon: React.FC<IconProps> = observer(({size = 16}) => (
	<PaperPlaneRightIcon
		size={remFromPx(size)}
		weight="fill"
		data-flx="ui.action-menu.context-menu-icons.send-invite-to-community-icon.paper-plane-right-icon"
	/>
));
export const LocalMuteIcon: React.FC<IconProps> = observer(({size = 16, className}) => (
	<SpeakerSlashIcon
		size={remFromPx(size)}
		weight="fill"
		className={className}
		data-flx="ui.action-menu.context-menu-icons.local-mute-icon.speaker-slash-icon"
	/>
));
export const LocalDisableVideoIcon: React.FC<IconProps> = observer(({size = 16, className}) => (
	<VideoCameraSlashIcon
		size={remFromPx(size)}
		weight="fill"
		className={className}
		data-flx="ui.action-menu.context-menu-icons.local-disable-video-icon.video-camera-slash-icon"
	/>
));
export const GuildMuteIcon: React.FC<IconProps> = observer(({size = 16, className}) => (
	<MicrophoneSlashIcon
		size={remFromPx(size)}
		weight="fill"
		className={className}
		data-flx="ui.action-menu.context-menu-icons.guild-mute-icon.microphone-slash-icon"
	/>
));
export const GuildDeafenIcon: React.FC<IconProps> = observer(({size = 16, className}) => (
	<SpeakerSlashIcon
		size={remFromPx(size)}
		weight="fill"
		className={className}
		data-flx="ui.action-menu.context-menu-icons.guild-deafen-icon.speaker-slash-icon"
	/>
));
export const BulkTurnOffCameraIcon: React.FC<IconProps> = observer(({size = 16, className}) => (
	<VideoIcon
		size={remFromPx(size)}
		weight="fill"
		className={className}
		data-flx="ui.action-menu.context-menu-icons.bulk-turn-off-camera-icon.video-icon"
	/>
));
export const DebugChannelIcon: React.FC<IconProps> = observer(({size = 16, className}) => (
	<BugBeetleIcon
		size={remFromPx(size)}
		weight="fill"
		className={className}
		data-flx="ui.action-menu.context-menu-icons.debug-channel-icon.bug-beetle-icon"
	/>
));
export const CopyMediaIcon: React.FC<IconProps> = observer(({size = 16, className}) => (
	<CopySimpleIcon
		size={remFromPx(size)}
		weight="fill"
		className={className}
		data-flx="ui.action-menu.context-menu-icons.copy-media-icon.copy-simple-icon"
	/>
));
export const DownloadMediaIcon: React.FC<IconProps> = observer(({size = 16, className}) => (
	<DownloadSimpleIcon
		size={remFromPx(size)}
		weight="fill"
		className={className}
		data-flx="ui.action-menu.context-menu-icons.download-media-icon.download-simple-icon"
	/>
));
export const OpenMediaLinkIcon: React.FC<IconProps> = observer(({size = 16, className}) => (
	<ArrowSquareOutIcon
		size={remFromPx(size)}
		weight="regular"
		className={className}
		data-flx="ui.action-menu.context-menu-icons.open-media-link-icon.arrow-square-out-icon"
	/>
));
