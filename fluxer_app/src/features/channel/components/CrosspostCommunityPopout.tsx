// SPDX-License-Identifier: AGPL-3.0-or-later

import {CrosspostCommunityCard} from '@app/features/channel/components/CrosspostCommunityCard';
import type {Message} from '@app/features/messaging/models/MessagingMessage';
import {WebhookContextMenu} from '@app/features/ui/action_menu/WebhookContextMenu';
import {BottomSheet} from '@app/features/ui/bottom_sheet/BottomSheet';
import * as ContextMenuCommands from '@app/features/ui/commands/ContextMenuCommands';
import {Popout} from '@app/features/ui/popover/PopoverPopout';
import MobileLayout from '@app/features/ui/state/MobileLayout';
import {observer} from 'mobx-react-lite';
import React, {useCallback, useState} from 'react';

type CrosspostChildProps = React.HTMLAttributes<HTMLElement> & React.RefAttributes<HTMLElement>;

interface CrosspostCommunityPopoutProps {
	message: Message;
	children: React.ReactElement<CrosspostChildProps>;
	onPopoutOpen?: () => void;
	onPopoutClose?: () => void;
}

export const CrosspostCommunityPopout = observer(function CrosspostCommunityPopout({
	message,
	children,
	onPopoutOpen,
	onPopoutClose,
}: CrosspostCommunityPopoutProps) {
	const [sheetOpen, setSheetOpen] = useState(false);
	const webhookId = message.webhookId;
	const handleContextMenu = useCallback(
		(event: React.MouseEvent<HTMLElement>) => {
			if (!webhookId) return;
			event.preventDefault();
			event.stopPropagation();
			ContextMenuCommands.openFromEvent(event, ({onClose}) => (
				<WebhookContextMenu
					webhookId={webhookId}
					onClose={onClose}
					data-flx="channel.crosspost-community-popout.handle-context-menu.webhook-context-menu"
				/>
			));
		},
		[webhookId],
	);
	const handleSheetClose = useCallback(() => setSheetOpen(false), []);
	if (MobileLayout.enabled) {
		const {onClick: originalOnClick} = children.props;
		const clonedChild = React.cloneElement(children, {
			onClick: (event: React.MouseEvent<HTMLElement>) => {
				originalOnClick?.(event);
				setSheetOpen(true);
			},
			onContextMenu: handleContextMenu,
		});
		return (
			<>
				{clonedChild}
				{sheetOpen && (
					<BottomSheet
						isOpen={true}
						onClose={handleSheetClose}
						disablePadding={true}
						disableDefaultHeader={true}
						data-flx="channel.crosspost-community-popout.bottom-sheet"
					>
						<CrosspostCommunityCard
							message={message}
							onClose={handleSheetClose}
							variant="sheet"
							data-flx="channel.crosspost-community-popout.crosspost-community-card"
						/>
					</BottomSheet>
				)}
			</>
		);
	}
	return (
		<Popout
			render={({onClose}) => (
				<CrosspostCommunityCard
					message={message}
					onClose={onClose}
					data-flx="channel.crosspost-community-popout.crosspost-community-card--2"
				/>
			)}
			position="right-start"
			animationType="profile-slide"
			constrainHeight={false}
			freezePosition
			keepOpenOnTargetUnmount
			onOpen={onPopoutOpen}
			onClose={onPopoutClose}
			data-flx="channel.crosspost-community-popout.popout"
		>
			{React.cloneElement(children, {onContextMenu: handleContextMenu})}
		</Popout>
	);
});
