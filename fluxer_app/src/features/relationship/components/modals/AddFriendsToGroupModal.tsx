// SPDX-License-Identifier: AGPL-3.0-or-later

import * as Modal from '@app/features/app/components/dialogs/Modal';
import {CopyLinkSection} from '@app/features/app/components/dialogs/shared/CopyLinkSection';
import selectorStyles from '@app/features/app/components/dialogs/shared/SelectorModalStyles.module.css';
import {FriendSelector} from '@app/features/app/components/shared/FriendSelector';
import {useAddFriendsToGroupModalLogic} from '@app/features/channel/utils/AddFriendsToGroupModalUtils';
import {CREATE_DESCRIPTOR, SEARCH_FRIENDS_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import inviteStyles from '@app/features/invite/components/modals/InviteModal.module.css';
import StreamerMode from '@app/features/streamer_mode/state/StreamerMode';
import {remFromPx} from '@app/features/theme/layout/RemFromPx';
import {Button} from '@app/features/ui/button/Button';
import {Input} from '@app/features/ui/components/form/FormInput';
import {msg} from '@lingui/core/macro';
import {Trans, useLingui} from '@lingui/react/macro';
import {MagnifyingGlassIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';

const SELECT_FRIENDS_DESCRIPTOR = msg({
	message: 'Select friends',
	comment: 'Button or menu action label in the add friends to group modal. Keep it concise.',
});
const GENERATE_INVITE_LINK_DESCRIPTOR = msg({
	message: 'Generate invite link',
	comment: 'Short label in the add friends to group modal. Keep it concise.',
});
const LINK_HIDDEN_WHILE_SHARING_DESCRIPTOR = msg({
	message: 'Link hidden while sharing',
	comment: 'Replacement text for an invite URL while streaming privacy is active.',
});
const INVITE_LINK_HIDDEN_LABEL_DESCRIPTOR = msg({
	message: 'Invite link is hidden while sharing:',
	comment: 'Label above a masked invite link while streaming privacy is active.',
});

interface AddFriendsToGroupModalProps {
	channelId: string;
}

export const AddFriendsToGroupModal = observer((props: AddFriendsToGroupModalProps) => {
	const {i18n} = useLingui();
	const modalLogic = useAddFriendsToGroupModalLogic(props.channelId);
	const hasSelection = modalLogic.selectedUserIds.length > 0;
	const canAddFriends = hasSelection && !modalLogic.isAdding;
	const hideInviteLinks = StreamerMode.shouldHideInviteLinks;
	const displayedInviteLink =
		hideInviteLinks && modalLogic.inviteLink
			? i18n._(LINK_HIDDEN_WHILE_SHARING_DESCRIPTOR)
			: (modalLogic.inviteLink ?? '');
	return (
		<Modal.Root size="small" centered data-flx="relationship.add-friends-to-group-modal.modal-root">
			<Modal.Header
				title={i18n._(SELECT_FRIENDS_DESCRIPTOR)}
				data-flx="relationship.add-friends-to-group-modal.modal-header"
			>
				<div className={selectorStyles.headerSearch} data-flx="relationship.add-friends-to-group-modal.div">
					<Input
						value={modalLogic.searchQuery}
						onChange={(e) => modalLogic.setSearchQuery(e.target.value)}
						placeholder={i18n._(SEARCH_FRIENDS_DESCRIPTOR)}
						leftIcon={
							<MagnifyingGlassIcon
								size={remFromPx(20)}
								weight="bold"
								className={selectorStyles.searchIcon}
								data-flx="relationship.add-friends-to-group-modal.magnifying-glass-icon"
							/>
						}
						className={selectorStyles.headerSearchInput}
						rightElement={
							<Button
								onClick={modalLogic.handleAddFriends}
								disabled={!canAddFriends}
								submitting={modalLogic.isAdding}
								compact
								fitContent
								data-flx="relationship.add-friends-to-group-modal.button.add-friends"
							>
								<Trans>Add</Trans>
							</Button>
						}
						data-flx="relationship.add-friends-to-group-modal.input.set-search-query"
					/>
				</div>
			</Modal.Header>
			<Modal.Content
				className={selectorStyles.selectorContent}
				data-flx="relationship.add-friends-to-group-modal.modal-content"
			>
				<FriendSelector
					selectedUserIds={modalLogic.selectedUserIds}
					onToggle={modalLogic.handleToggle}
					maxSelections={modalLogic.remainingSlotsCount}
					excludeUserIds={modalLogic.currentMemberIds}
					searchQuery={modalLogic.searchQuery}
					onSearchQueryChange={modalLogic.setSearchQuery}
					showSearchInput={false}
					data-flx="relationship.add-friends-to-group-modal.friend-selector"
				/>
			</Modal.Content>
			<Modal.Footer data-flx="relationship.add-friends-to-group-modal.modal-footer">
				<CopyLinkSection
					label={
						hideInviteLinks ? (
							i18n._(INVITE_LINK_HIDDEN_LABEL_DESCRIPTOR)
						) : (
							<Trans>or send an invite to a friend:</Trans>
						)
					}
					value={displayedInviteLink}
					onCopy={modalLogic.inviteLink && !hideInviteLinks ? modalLogic.handleGenerateOrCopyInvite : undefined}
					copyDisabled={modalLogic.isGeneratingInvite || hideInviteLinks}
					inputProps={{placeholder: i18n._(GENERATE_INVITE_LINK_DESCRIPTOR)}}
					rightElement={
						!modalLogic.inviteLink ? (
							<Button
								onClick={hideInviteLinks ? modalLogic.handleGenerateInvite : modalLogic.handleGenerateOrCopyInvite}
								submitting={modalLogic.isGeneratingInvite}
								compact
								fitContent
								data-flx="relationship.add-friends-to-group-modal.button.generate-or-copy-invite"
							>
								{i18n._(CREATE_DESCRIPTOR)}
							</Button>
						) : undefined
					}
					data-flx="relationship.add-friends-to-group-modal.copy-link-section"
				>
					<p className={inviteStyles.expirationText} data-flx="relationship.add-friends-to-group-modal.p--2">
						<Trans>Your invite expires in 24 hours</Trans>
					</p>
				</CopyLinkSection>
			</Modal.Footer>
		</Modal.Root>
	);
});

AddFriendsToGroupModal.displayName = 'AddFriendsToGroupModal';
