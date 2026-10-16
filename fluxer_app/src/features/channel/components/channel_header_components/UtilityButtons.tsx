// SPDX-License-Identifier: AGPL-3.0-or-later

import {ChannelHeaderIcon} from '@app/features/channel/components/channel_header_components/ChannelHeaderIcon';
import {DeveloperToolsContextMenu} from '@app/features/channel/components/channel_header_components/DeveloperToolsContextMenu';
import DeveloperMode from '@app/features/devtools/state/DeveloperMode';
import {INBOX_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {InboxPopout} from '@app/features/messaging/components/popouts/InboxPopout';
import * as ContextMenuCommands from '@app/features/ui/commands/ContextMenuCommands';
import {FluxerStaffIcon} from '@app/features/ui/components/icons/FluxerStaffIcon';
import {InboxIcon} from '@app/features/ui/components/icons/InboxIcon';
import {useContextMenuTrigger} from '@app/features/ui/hooks/useContextMenuTrigger';
import {usePopout} from '@app/features/ui/hooks/usePopout';
import {Popout} from '@app/features/ui/popover/PopoverPopout';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useCallback} from 'react';

const DEVELOPER_TOOLS_DESCRIPTOR = msg({
	message: 'Developer tools',
	comment: 'Short label in the channel and chat utility buttons. Keep it concise.',
});

export function isStaffToolsButtonVisible(): boolean {
	return DeveloperMode.isDeveloper;
}

interface HeaderUtilityButtonProps {
	className?: string;
	'data-flx'?: string;
}

export const StaffToolsButton = observer(({className, ...rest}: HeaderUtilityButtonProps) => {
	const {i18n} = useLingui();
	const {isOpen, withTracking} = useContextMenuTrigger();
	const handleOpenTools = useCallback(
		(event: React.MouseEvent<HTMLButtonElement>) => {
			ContextMenuCommands.openFromElementBottomRight(
				event,
				({onClose}) => (
					<DeveloperToolsContextMenu
						onClose={onClose}
						data-flx="channel.channel-header-components.utility-buttons.handle-open-tools.developer-tools-context-menu"
					/>
				),
				withTracking(),
			);
		},
		[withTracking],
	);
	if (!isStaffToolsButtonVisible()) {
		return null;
	}
	return (
		<ChannelHeaderIcon
			icon={FluxerStaffIcon}
			label={i18n._(DEVELOPER_TOOLS_DESCRIPTOR)}
			className={className}
			isSelected={isOpen}
			aria-haspopup="menu"
			aria-expanded={isOpen}
			onClick={handleOpenTools}
			onContextMenu={handleOpenTools}
			data-flx="channel.channel-header-components.utility-buttons.staff-tools-button.channel-header-icon.open-tools"
			{...rest}
		/>
	);
});
export const InboxButton = observer(({className, ...rest}: HeaderUtilityButtonProps) => {
	const {i18n} = useLingui();
	const {isOpen, openProps} = usePopout('inbox');
	return (
		<Popout
			data-flx="channel.channel-header-components.utility-buttons.inbox-button.popout"
			{...openProps}
			render={() => (
				<InboxPopout data-flx="channel.channel-header-components.utility-buttons.inbox-button.inbox-popout" />
			)}
			position="bottom-end"
			subscribeTo="INBOX_OPEN"
		>
			<ChannelHeaderIcon
				icon={InboxIcon}
				label={i18n._(INBOX_DESCRIPTOR)}
				className={className}
				isSelected={isOpen}
				aria-haspopup={true}
				aria-expanded={isOpen}
				keybindAction="chat_toggle_inbox"
				data-flx="channel.channel-header-components.utility-buttons.inbox-button.channel-header-icon"
				{...rest}
			/>
		</Popout>
	);
});
