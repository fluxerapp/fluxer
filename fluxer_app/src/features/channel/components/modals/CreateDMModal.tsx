// SPDX-License-Identifier: AGPL-3.0-or-later

import * as Modal from '@app/features/app/components/dialogs/Modal';
import selectorStyles from '@app/features/app/components/dialogs/shared/SelectorModalStyles.module.css';
import {FriendSelector} from '@app/features/app/components/shared/FriendSelector';
import {
	type CreateDMModalProps,
	CreateDMRestrictionSlate,
	DUPLICATE_GROUP_MODAL_KEY,
	useCreateDMModalLogic,
} from '@app/features/channel/utils/CreateDMModalUtils';
import {DuplicateGroupConfirmModal} from '@app/features/guild/components/modals/DuplicateGroupConfirmModal';
import {SEARCH_FRIENDS_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {Button} from '@app/features/ui/button/Button';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {modal} from '@app/features/ui/commands/ModalCommands';
import {Input} from '@app/features/ui/components/form/FormInput';
import {msg} from '@lingui/core/macro';
import {Trans, useLingui} from '@lingui/react/macro';
import {MagnifyingGlassIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import {useCallback} from 'react';

const SELECT_FRIENDS_DESCRIPTOR = msg({
	message: 'Select friends',
	comment: 'Button or menu action label in the create dm modal. Keep it concise.',
});
export const CreateDMModal = observer((props: CreateDMModalProps) => {
	const {i18n} = useLingui();
	const modalLogic = useCreateDMModalLogic(props);
	const restriction = modalLogic.restriction;
	const handleCreate = useCallback(async () => {
		const result = await modalLogic.handleCreate();
		if (result && result.duplicates.length > 0) {
			ModalCommands.pushWithKey(
				modal(() => (
					<DuplicateGroupConfirmModal
						channels={result.duplicates}
						onConfirm={() => modalLogic.handleCreateChannel(result.selectionSnapshot)}
						data-flx="channel.create-dm-modal.handle-create.duplicate-group-confirm-modal"
					/>
				)),
				DUPLICATE_GROUP_MODAL_KEY,
			);
		}
	}, [modalLogic]);
	return (
		<Modal.Root
			size="small"
			centered
			className={selectorStyles.selectorRoot}
			data-flx="channel.create-dm-modal.modal-root"
		>
			<Modal.Header title={i18n._(SELECT_FRIENDS_DESCRIPTOR)} data-flx="channel.create-dm-modal.modal-header">
				{!restriction && (
					<>
						<p className={selectorStyles.subtitle} data-flx="channel.create-dm-modal.p">
							{modalLogic.subtitleText}
						</p>
						<div className={selectorStyles.headerSearch} data-flx="channel.create-dm-modal.div">
							<Input
								value={modalLogic.searchQuery}
								onChange={(e) => modalLogic.setSearchQuery(e.target.value)}
								placeholder={i18n._(SEARCH_FRIENDS_DESCRIPTOR)}
								leftIcon={
									<MagnifyingGlassIcon
										weight="bold"
										className={selectorStyles.searchIcon}
										data-flx="channel.create-dm-modal.magnifying-glass-icon"
									/>
								}
								className={selectorStyles.headerSearchInput}
								data-flx="channel.create-dm-modal.input.set-search-query"
							/>
						</div>
					</>
				)}
			</Modal.Header>
			<Modal.Content className={selectorStyles.selectorContent} data-flx="channel.create-dm-modal.modal-content">
				{restriction ? (
					<CreateDMRestrictionSlate restriction={restriction} data-flx="channel.create-dm-modal.restriction-slate" />
				) : (
					<FriendSelector
						selectedUserIds={modalLogic.selectedUserIds}
						onToggle={modalLogic.handleToggle}
						maxSelections={modalLogic.maxSelections}
						searchQuery={modalLogic.searchQuery}
						onSearchQueryChange={modalLogic.setSearchQuery}
						showSearchInput={false}
						stickyUserIds={props.initialSelectedUserIds}
						data-flx="channel.create-dm-modal.friend-selector"
					/>
				)}
			</Modal.Content>
			<Modal.Footer className={selectorStyles.footer} data-flx="channel.create-dm-modal.modal-footer">
				<Button variant="secondary" onClick={() => ModalCommands.pop()} data-flx="channel.create-dm-modal.button.pop">
					<Trans>Cancel</Trans>
				</Button>
				{!restriction && (
					<Button
						onClick={handleCreate}
						disabled={modalLogic.isCreating}
						submitting={modalLogic.isCreating}
						data-flx="channel.create-dm-modal.button.create"
					>
						{modalLogic.buttonText}
					</Button>
				)}
			</Modal.Footer>
		</Modal.Root>
	);
});
