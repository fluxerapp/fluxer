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
import React from 'react';
import * as ModalActionCreators from '~/actions/ModalActionCreators';
import {modal} from '~/actions/ModalActionCreators';
import {FriendSelector} from '~/components/common/FriendSelector';
import {Input} from '~/components/form/Input';
import {DuplicateGroupConfirmModal} from '~/components/modals/DuplicateGroupConfirmModal';
import * as Modal from '~/components/modals/Modal';
import selectorStyles from '~/components/modals/shared/SelectorModalStyles.module.css';
import {Button} from '~/components/uikit/Button/Button';
import {type CreateDMModalProps, useCreateDMModalLogic} from '~/utils/modals/CreateDMModalUtils';

export const CreateDMModal = observer((props: CreateDMModalProps) => {
	const {t} = useLingui();
	const modalLogic = useCreateDMModalLogic(props);

	const handleCreate = React.useCallback(async () => {
		const result = await modalLogic.handleCreate();
		if (result && result.duplicates.length > 0) {
			ModalActionCreators.push(
				modal(() => (
					<DuplicateGroupConfirmModal
						channels={result.duplicates}
						onConfirm={() => modalLogic.handleCreateChannel(result.selectionSnapshot)}
					/>
				)),
			);
		}
	}, [modalLogic]);

	return (
		<Modal.Root size="small" centered>
			<Modal.Header title={t`Select Friends`}>
				<p className={selectorStyles.subtitle}>
					{modalLogic.selectedUserIds.length === 0 ? (
						<Trans>You can add up to {modalLogic.maxSelections} friends</Trans>
					) : (
						<Trans>
							You can add {Math.max(0, modalLogic.maxSelections - modalLogic.selectedUserIds.length)} more friends
						</Trans>
					)}
				</p>
				<div className={selectorStyles.headerSearch}>
					<Input
						value={modalLogic.searchQuery}
						onChange={(e) => modalLogic.setSearchQuery(e.target.value)}
						placeholder={t`Search friends`}
						leftIcon={<MagnifyingGlassIcon weight="bold" className={selectorStyles.searchIcon} />}
						className={selectorStyles.headerSearchInput}
					/>
				</div>
			</Modal.Header>
			<Modal.Content className={selectorStyles.selectorContent}>
				<FriendSelector
					selectedUserIds={modalLogic.selectedUserIds}
					onToggle={modalLogic.handleToggle}
					maxSelections={modalLogic.maxSelections}
					searchQuery={modalLogic.searchQuery}
					onSearchQueryChange={modalLogic.setSearchQuery}
					showSearchInput={false}
					stickyUserIds={props.initialSelectedUserIds}
				/>
			</Modal.Content>
			<Modal.Footer className={selectorStyles.footer}>
				<div className={selectorStyles.actionRow}>
					<Button variant="secondary" onClick={() => ModalActionCreators.pop()} className={selectorStyles.actionButton}>
						<Trans>Cancel</Trans>
					</Button>
					<Button
						onClick={handleCreate}
						disabled={modalLogic.isCreating}
						submitting={modalLogic.isCreating}
						className={selectorStyles.actionButton}
					>
						{modalLogic.buttonText}
					</Button>
				</div>
			</Modal.Footer>
		</Modal.Root>
	);
});
