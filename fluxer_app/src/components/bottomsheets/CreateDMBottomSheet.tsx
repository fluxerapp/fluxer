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
import {observer} from 'mobx-react-lite';
import React from 'react';
import * as ModalActionCreators from '~/actions/ModalActionCreators';
import {modal} from '~/actions/ModalActionCreators';
import {FriendSelector} from '~/components/common/FriendSelector';
import {DuplicateGroupConfirmModal} from '~/components/modals/DuplicateGroupConfirmModal';
import {BottomSheet} from '~/components/uikit/BottomSheet/BottomSheet';
import {Button} from '~/components/uikit/Button/Button';
import {Scroller} from '~/components/uikit/Scroller';
import {useCreateDMModalLogic} from '~/utils/modals/CreateDMModalUtils';
import styles from './CreateDMBottomSheet.module.css';

interface CreateDMBottomSheetProps {
	isOpen: boolean;
	onClose: () => void;
}

type ScrollContentStyle = React.CSSProperties & {
	'--create-dm-scroll-padding-bottom'?: string;
};

export const CreateDMBottomSheet = observer(({isOpen, onClose}: CreateDMBottomSheetProps) => {
	const {t} = useLingui();
	const modalLogic = useCreateDMModalLogic({autoCloseOnCreate: false, resetKey: isOpen});
	const snapPoints = React.useMemo(() => [0, 1], []);
	const footerRef = React.useRef<HTMLDivElement>(null);
	const [footerHeight, setFooterHeight] = React.useState(0);

	React.useLayoutEffect(() => {
		if (!isOpen) {
			setFooterHeight(0);
			return undefined;
		}

		const element = footerRef.current;
		if (!element) {
			return undefined;
		}

		const updateHeight = () => setFooterHeight(element.offsetHeight);
		updateHeight();

		const resizeObserver = typeof ResizeObserver !== 'undefined' ? new ResizeObserver(updateHeight) : null;
		if (resizeObserver) {
			resizeObserver.observe(element);
		}

		const handleResize = () => updateHeight();
		if (typeof window !== 'undefined') {
			window.addEventListener('resize', handleResize);
		}

		return () => {
			resizeObserver?.disconnect();
			if (typeof window !== 'undefined') {
				window.removeEventListener('resize', handleResize);
			}
		};
	}, [isOpen]);

	const scrollContentStyle = React.useMemo<ScrollContentStyle>(() => {
		if (footerHeight === 0) {
			return {};
		}
		return {'--create-dm-scroll-padding-bottom': `calc(${footerHeight}px + 16px)`};
	}, [footerHeight]);

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
			return;
		}

		onClose();
	}, [modalLogic, onClose]);

	return (
		<BottomSheet isOpen={isOpen} onClose={onClose} snapPoints={snapPoints} title={t`Select Friends`} disablePadding>
			<div className={styles.container}>
				<Scroller className={styles.scroller} fade={false}>
					<div className={styles.content} style={scrollContentStyle}>
						<p className={styles.description}>
							{modalLogic.selectedUserIds.length === 0 ? (
								<Trans>You can add up to {modalLogic.maxSelections} friends</Trans>
							) : (
								<Trans>
									You can add {Math.max(0, modalLogic.maxSelections - modalLogic.selectedUserIds.length)} more friends
								</Trans>
							)}
						</p>
						<div className={styles.friendSelector}>
							<FriendSelector
								selectedUserIds={modalLogic.selectedUserIds}
								onToggle={modalLogic.handleToggle}
								maxSelections={modalLogic.maxSelections}
								searchQuery={modalLogic.searchQuery}
								onSearchQueryChange={modalLogic.setSearchQuery}
							/>
						</div>
					</div>
				</Scroller>

				<div className={styles.footer} ref={footerRef}>
					<Button variant="secondary" className={styles.cancelButton} onClick={onClose}>
						<Trans>Cancel</Trans>
					</Button>
					<Button
						variant="primary"
						className={styles.createButton}
						onClick={handleCreate}
						disabled={modalLogic.isCreating}
						submitting={modalLogic.isCreating}
					>
						{modalLogic.buttonText}
					</Button>
				</div>
			</div>
		</BottomSheet>
	);
});
