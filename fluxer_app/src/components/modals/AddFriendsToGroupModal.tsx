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
import {MagnifyingGlassIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import {FriendSelector} from '~/components/common/FriendSelector';
import {Input} from '~/components/form/Input';
import inviteStyles from '~/components/modals/InviteModal.module.css';
import * as Modal from '~/components/modals/Modal';
import {CopyLinkSection} from '~/components/modals/shared/CopyLinkSection';
import selectorStyles from '~/components/modals/shared/SelectorModalStyles.module.css';
import {Button} from '~/components/uikit/Button/Button';
import {useAddFriendsToGroupModalLogic} from '~/utils/modals/AddFriendsToGroupModalUtils';

interface AddFriendsToGroupModalProps {
	channelId: string;
}

export const AddFriendsToGroupModal = observer((props: AddFriendsToGroupModalProps) => {
	const {t} = useLingui();
	const modalLogic = useAddFriendsToGroupModalLogic(props.channelId);

	const hasSelection = modalLogic.selectedUserIds.length > 0;
	const canAddFriends = hasSelection && !modalLogic.isAdding;

	return (
		<Modal.Root size="small" centered>
			<Modal.Header title={t`Select Friends`}>
				<p className={selectorStyles.subtitle}>
					<Trans>You can add {modalLogic.remainingSlotsCount} more friends</Trans>
				</p>
				<div className={selectorStyles.headerSearch}>
					<Input
						value={modalLogic.searchQuery}
						onChange={(e) => modalLogic.setSearchQuery(e.target.value)}
						placeholder={t`Search friends`}
						leftIcon={<MagnifyingGlassIcon size={20} weight="bold" className={selectorStyles.searchIcon} />}
						className={selectorStyles.headerSearchInput}
						rightElement={
							<Button
								onClick={modalLogic.handleAddFriends}
								disabled={!canAddFriends}
								submitting={modalLogic.isAdding}
								compact
								fitContent
							>
								<Trans>Add</Trans>
							</Button>
						}
					/>
				</div>
			</Modal.Header>

			<Modal.Content className={selectorStyles.selectorContent}>
				<FriendSelector
					selectedUserIds={modalLogic.selectedUserIds}
					onToggle={modalLogic.handleToggle}
					maxSelections={modalLogic.remainingSlotsCount}
					excludeUserIds={modalLogic.currentMemberIds}
					searchQuery={modalLogic.searchQuery}
					onSearchQueryChange={modalLogic.setSearchQuery}
					showSearchInput={false}
				/>
			</Modal.Content>

			<Modal.Footer>
				<CopyLinkSection
					label={<Trans>or send an invite to a friend:</Trans>}
					value={modalLogic.inviteLink ?? ''}
					onCopy={modalLogic.inviteLink ? modalLogic.handleGenerateOrCopyInvite : undefined}
					copied={modalLogic.inviteLinkCopied}
					copyDisabled={modalLogic.isGeneratingInvite}
					inputProps={{placeholder: t`Generate invite link`}}
					rightElement={
						!modalLogic.inviteLink ? (
							<Button
								onClick={modalLogic.handleGenerateOrCopyInvite}
								submitting={modalLogic.isGeneratingInvite}
								compact
								fitContent
							>
								<Trans>Create</Trans>
							</Button>
						) : undefined
					}
				>
					<p className={inviteStyles.expirationText}>
						<Trans>Your invite expires in 24 hours</Trans>
					</p>
				</CopyLinkSection>
			</Modal.Footer>
		</Modal.Root>
	);
});

AddFriendsToGroupModal.displayName = 'AddFriendsToGroupModal';
