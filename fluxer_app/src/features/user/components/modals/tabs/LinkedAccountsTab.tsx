// SPDX-License-Identifier: AGPL-3.0-or-later

import {ConfirmModal} from '@app/features/app/components/dialogs/ConfirmModal';
import {SettingsSection} from '@app/features/app/components/dialogs/shared/SettingsSection';
import {SettingsTabContainer, SettingsTabContent} from '@app/features/app/components/dialogs/shared/SettingsTabLayout';
import {StatusSlate} from '@app/features/app/components/dialogs/shared/StatusSlate';
import {computeVerticalDropPosition} from '@app/features/app/components/layout/dnd/DndDropPosition';
import type {ConnectionDragItem} from '@app/features/app/components/layout/types/DndTypes';
import {DND_TYPES} from '@app/features/app/components/layout/types/DndTypes';
import {BLUESKY_PROVIDER_NAME, PRODUCT_NAME} from '@app/features/app/config/I18nDisplayConstants';
import {useMergeRefs} from '@app/features/app/hooks/useMergeRefs';
import RuntimeConfig from '@app/features/app/state/RuntimeConfig';
import * as ConnectionCommands from '@app/features/connection/commands/ConnectionCommands';
import {AddConnectionModal} from '@app/features/connection/components/modals/AddConnectionModal';
import {EditConnectionModal} from '@app/features/connection/components/modals/EditConnectionModal';
import type {Connection} from '@app/features/connection/models/Connection';
import UserConnection from '@app/features/connection/state/UserConnection';
import {CONNECTIONS_DESCRIPTOR, DOMAIN_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {StreamerModeGate} from '@app/features/streamer_mode/components/StreamerModeGate';
import StreamerMode from '@app/features/streamer_mode/state/StreamerMode';
import {remFromPx} from '@app/features/theme/layout/RemFromPx';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {modal} from '@app/features/ui/commands/ModalCommands';
import {BlueskyIcon} from '@app/features/ui/components/icons/BlueskyIcon';
import {UnverifiedConnectionIcon} from '@app/features/ui/components/icons/UnverifiedConnectionIcon';
import {VerifiedConnectionIcon} from '@app/features/ui/components/icons/VerifiedConnectionIcon';
import {Spinner} from '@app/features/ui/components/Spinner';
import {useDragAutoScroll} from '@app/features/ui/hooks/useDragAutoScroll';
import {Tooltip} from '@app/features/ui/tooltip/Tooltip';
import styles from '@app/features/user/components/modals/tabs/LinkedAccountsTab.module.css';
import {type ConnectionType, ConnectionTypes} from '@fluxer/constants/src/ConnectionConstants';
import {msg} from '@lingui/core/macro';
import {Trans, useLingui} from '@lingui/react/macro';
import {DotsSixVerticalIcon, GlobeSimpleIcon, PencilSimpleIcon, TrashIcon, UserListIcon} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useCallback, useEffect, useMemo, useRef, useState} from 'react';
import type {ConnectableElement} from 'react-dnd';
import {DndProvider, useDrag, useDragLayer, useDrop} from 'react-dnd';
import {getEmptyImage, HTML5Backend} from 'react-dnd-html5-backend';

const THIS_CONNECTION_HAS_BEEN_VERIFIED_DESCRIPTOR = msg({
	message: 'This connection has been verified.',
	comment: 'Description text in the linked accounts tab.',
});
const THIS_CONNECTION_HAS_NOT_BEEN_VERIFIED_DESCRIPTOR = msg({
	message: 'This connection has not been verified.',
	comment: 'Description text in the linked accounts tab.',
});
const EDIT_DESCRIPTOR = msg({
	message: 'Edit',
	comment: 'Button or menu action label in the linked accounts tab. Keep it concise.',
});
const REMOVE_DESCRIPTOR = msg({
	message: 'Remove',
	comment: 'Button or menu action label in the linked accounts tab. Keep it concise. Keep the tone plain and specific.',
});
const REMOVE_CONNECTION_DESCRIPTOR = msg({
	message: 'Remove connection',
	comment: 'Button or menu action label in the linked accounts tab. Keep it concise. Keep the tone plain and specific.',
});
const ARE_YOU_SURE_YOU_WANT_TO_REMOVE_THIS_DESCRIPTOR = msg({
	message: "Remove this connection? Can't be undone.",
	comment: 'Error message in the linked accounts tab. Keep the tone plain and specific.',
});
const ADD_CONNECTION_DESCRIPTOR = msg({
	message: 'Add {blueskyProviderName} connection',
	comment:
		'Button or menu action label in the linked accounts tab. Keep it concise. Preserve {blueskyProviderName}; it is inserted by code.',
});
const ADD_DOMAIN_CONNECTION_DESCRIPTOR = msg({
	message: 'Add domain connection',
	comment: 'Button or menu action label in the linked accounts tab. Keep it concise.',
});

interface ConnectionCardProps {
	connection: Connection;
	index: number;
	onDelete: () => void;
	onEdit: () => void;
	onMoveConnection: (dragIndex: number, hoverIndex: number) => void;
	onDropConnection: () => void;
}

const ConnectionDragAutoScroll: React.FC<{listRef: React.RefObject<HTMLDivElement | null>}> = ({listRef}) => {
	const isDraggingConnection = useDragLayer(
		(monitor) => monitor.isDragging() && monitor.getItemType() === DND_TYPES.CONNECTION,
	);
	const getScrollElement = useCallback(() => {
		const list = listRef.current;
		if (list == null) return null;
		return list.closest<HTMLElement>('[data-fluxer-scroll-container="true"]');
	}, [listRef]);
	useDragAutoScroll({active: isDraggingConnection, getScrollElement});
	return null;
};

const ConnectionCard: React.FC<ConnectionCardProps> = observer(
	({connection, index, onDelete, onEdit, onMoveConnection, onDropConnection}) => {
		const {i18n} = useLingui();
		const [dropIndicator, setDropIndicator] = useState<'top' | 'bottom' | null>(null);
		const cardRef = useRef<HTMLDivElement>(null);
		const dragItemData = useMemo<ConnectionDragItem>(
			() => ({
				type: DND_TYPES.CONNECTION,
				id: connection.id,
				index,
			}),
			[connection.id, index],
		);
		const [{isDragging}, dragRef, preview] = useDrag(
			() => ({
				type: DND_TYPES.CONNECTION,
				item: () => dragItemData,
				collect: (monitor) => ({isDragging: monitor.isDragging()}),
				end: () => setDropIndicator(null),
			}),
			[dragItemData],
		);
		const [{isOver}, dropRef] = useDrop(
			() => ({
				accept: DND_TYPES.CONNECTION,
				hover: (item: ConnectionDragItem, monitor) => {
					if (item.id === connection.id) {
						setDropIndicator(null);
						return;
					}
					const node = cardRef.current;
					if (!node) return;
					const clientOffset = monitor.getClientOffset();
					if (!clientOffset) return;
					const boundingRect = node.getBoundingClientRect();
					const dropPos = computeVerticalDropPosition(clientOffset, boundingRect);
					setDropIndicator(dropPos === 'before' ? 'top' : 'bottom');
					const hoverIndex = index;
					const dragIndex = item.index;
					if (dragIndex === hoverIndex) return;
					onMoveConnection(dragIndex, hoverIndex);
					item.index = hoverIndex;
				},
				drop: () => {
					setDropIndicator(null);
					onDropConnection();
				},
				collect: (monitor) => ({
					isOver: monitor.isOver({shallow: true}),
				}),
			}),
			[connection.id, index, onMoveConnection, onDropConnection],
		);
		useEffect(() => {
			preview(getEmptyImage(), {captureDraggingState: true});
		}, [preview]);
		useEffect(() => {
			if (!isOver) {
				setDropIndicator(null);
			}
		}, [isOver]);
		const dragConnectorRef = useCallback(
			(node: ConnectableElement | null) => {
				dragRef(node);
			},
			[dragRef],
		);
		const dropConnectorRef = useCallback(
			(node: ConnectableElement | null) => {
				dropRef(node);
			},
			[dropRef],
		);
		const mergedRef = useMergeRefs([dropConnectorRef, cardRef]);
		const icon =
			connection.type === ConnectionTypes.BLUESKY ? (
				<BlueskyIcon size={20} data-flx="user.linked-accounts-tab.connection-card.bluesky-icon" />
			) : (
				<GlobeSimpleIcon
					size={remFromPx(20)}
					className={styles.domainIcon}
					data-flx="user.linked-accounts-tab.connection-card.domain-icon"
				/>
			);
		return (
			<div
				ref={mergedRef}
				className={clsx(
					styles.card,
					isDragging && styles.cardDragging,
					dropIndicator === 'top' && styles.dropIndicatorTop,
					dropIndicator === 'bottom' && styles.dropIndicatorBottom,
				)}
				data-flx="user.linked-accounts-tab.connection-card.card"
			>
				<div
					ref={dragConnectorRef}
					className={styles.cardDragHandle}
					data-flx="user.linked-accounts-tab.connection-card.card-drag-handle"
				>
					<DotsSixVerticalIcon
						size={remFromPx(20)}
						weight="bold"
						data-flx="user.linked-accounts-tab.connection-card.dots-six-vertical-icon"
					/>
				</div>
				<Tooltip
					text={connection.type === ConnectionTypes.BLUESKY ? BLUESKY_PROVIDER_NAME : i18n._(DOMAIN_DESCRIPTOR)}
					data-flx="user.linked-accounts-tab.connection-card.tooltip"
				>
					<div
						className={styles.cardIconSquircle}
						data-flx="user.linked-accounts-tab.connection-card.card-icon-squircle"
					>
						{icon}
					</div>
				</Tooltip>
				<div className={styles.cardInfo} data-flx="user.linked-accounts-tab.connection-card.card-info">
					<div className={styles.cardNameRow} data-flx="user.linked-accounts-tab.connection-card.card-name-row">
						<span className={styles.cardName} data-flx="user.linked-accounts-tab.connection-card.card-name">
							{connection.name}
						</span>
						<Tooltip
							text={
								connection.verified
									? i18n._(THIS_CONNECTION_HAS_BEEN_VERIFIED_DESCRIPTOR)
									: i18n._(THIS_CONNECTION_HAS_NOT_BEEN_VERIFIED_DESCRIPTOR)
							}
							data-flx="user.linked-accounts-tab.connection-card.tooltip--2"
						>
							<div
								className={styles.verificationBadge}
								data-flx="user.linked-accounts-tab.connection-card.verification-badge"
							>
								{connection.verified ? (
									<VerifiedConnectionIcon
										size={16}
										data-flx="user.linked-accounts-tab.connection-card.verified-connection-icon"
									/>
								) : (
									<UnverifiedConnectionIcon
										size={16}
										data-flx="user.linked-accounts-tab.connection-card.unverified-connection-icon"
									/>
								)}
							</div>
						</Tooltip>
					</div>
				</div>
				<div className={styles.cardActions} data-flx="user.linked-accounts-tab.connection-card.card-actions">
					<Tooltip text={i18n._(EDIT_DESCRIPTOR)} data-flx="user.linked-accounts-tab.connection-card.tooltip--3">
						<button
							type="button"
							className={styles.actionButton}
							onClick={onEdit}
							aria-label={i18n._(EDIT_DESCRIPTOR)}
							data-flx="user.linked-accounts-tab.connection-card.action-button.edit"
						>
							<PencilSimpleIcon
								size={remFromPx(16)}
								data-flx="user.linked-accounts-tab.connection-card.pencil-simple-icon"
							/>
						</button>
					</Tooltip>
					<Tooltip text={i18n._(REMOVE_DESCRIPTOR)} data-flx="user.linked-accounts-tab.connection-card.tooltip--4">
						<button
							type="button"
							className={styles.actionButton}
							onClick={onDelete}
							aria-label={i18n._(REMOVE_DESCRIPTOR)}
							data-flx="user.linked-accounts-tab.connection-card.action-button.delete"
						>
							<TrashIcon size={remFromPx(16)} data-flx="user.linked-accounts-tab.connection-card.trash-icon" />
						</button>
					</Tooltip>
				</div>
			</div>
		);
	},
);
const LinkedAccountsTab: React.FC = observer(() => {
	const {i18n} = useLingui();
	const [loaded, setLoaded] = useState(false);
	const [localOrder, setLocalOrder] = useState<ReadonlyArray<Connection> | null>(null);
	const connectionsListRef = useRef<HTMLDivElement>(null);
	const shouldGatePersonalDetails = StreamerMode.shouldHidePersonalInformation;
	useEffect(() => {
		if (shouldGatePersonalDetails) return;
		if (!loaded) {
			ConnectionCommands.fetchConnections().finally(() => setLoaded(true));
		}
	}, [loaded, shouldGatePersonalDetails]);
	useEffect(() => {
		if (shouldGatePersonalDetails) return;
		function handleVisibilityChange() {
			if (document.visibilityState === 'visible') {
				ConnectionCommands.fetchConnections();
			}
		}
		document.addEventListener('visibilitychange', handleVisibilityChange);
		return () => document.removeEventListener('visibilitychange', handleVisibilityChange);
	}, [shouldGatePersonalDetails]);
	const storeConnections = UserConnection.getConnections();
	const connections = localOrder ?? storeConnections;
	const handleMoveConnection = useCallback(
		(dragIndex: number, hoverIndex: number) => {
			const current = localOrder ? [...localOrder] : [...storeConnections];
			const dragged = current[dragIndex];
			current.splice(dragIndex, 1);
			current.splice(hoverIndex, 0, dragged);
			setLocalOrder(current);
		},
		[localOrder, storeConnections],
	);
	const handleDropConnection = useCallback(async () => {
		if (!localOrder) return;
		const connectionIds = localOrder.map((c) => c.id);
		setLocalOrder(null);
		await ConnectionCommands.reorderConnections(i18n, connectionIds);
	}, [i18n, localOrder]);
	const handleDelete = useCallback(
		(connection: Connection) => {
			ModalCommands.push(
				modal(() => (
					<ConfirmModal
						title={i18n._(REMOVE_CONNECTION_DESCRIPTOR)}
						description={i18n._(ARE_YOU_SURE_YOU_WANT_TO_REMOVE_THIS_DESCRIPTOR)}
						primaryText={i18n._(REMOVE_DESCRIPTOR)}
						primaryVariant="danger"
						onPrimary={async () => {
							await ConnectionCommands.deleteConnection(i18n, connection.type, connection.id);
						}}
						data-flx="user.linked-accounts-tab.handle-delete.confirm-modal"
					/>
				)),
			);
		},
		[i18n],
	);
	const handleEdit = useCallback((connection: Connection) => {
		ModalCommands.push(
			modal(() => (
				<EditConnectionModal
					connection={connection}
					data-flx="user.linked-accounts-tab.handle-edit.edit-connection-modal"
				/>
			)),
		);
	}, []);
	const handleAddConnection = useCallback((connectionType: ConnectionType) => {
		ModalCommands.push(
			modal(() => (
				<AddConnectionModal
					defaultType={connectionType}
					data-flx="user.linked-accounts-tab.handle-add-connection.add-connection-modal"
				/>
			)),
		);
	}, []);
	if (shouldGatePersonalDetails) {
		return (
			<SettingsTabContainer data-flx="user.linked-accounts-tab.settings-tab-container.streamer-mode-gate">
				<SettingsTabContent data-flx="user.linked-accounts-tab.settings-tab-content.streamer-mode-gate">
					<StreamerModeGate data-flx="user.linked-accounts-tab.streamer-mode-gate" />
				</SettingsTabContent>
			</SettingsTabContainer>
		);
	}
	if (!loaded) {
		return (
			<div className={styles.spinnerWrapper} data-flx="user.linked-accounts-tab.spinner-wrapper">
				<Spinner data-flx="user.linked-accounts-tab.spinner" />
			</div>
		);
	}
	return (
		<SettingsTabContainer data-flx="user.linked-accounts-tab.settings-tab-container">
			<SettingsTabContent data-flx="user.linked-accounts-tab.settings-tab-content">
				<SettingsSection
					id="connections"
					title={i18n._(CONNECTIONS_DESCRIPTOR)}
					description={
						<Trans>
							Link external accounts and domains to your {PRODUCT_NAME} profile. Verified connections will be displayed
							on your profile for others to see.
						</Trans>
					}
					data-flx="user.linked-accounts-tab.connections"
				>
					<div className={styles.platformRow} data-flx="user.linked-accounts-tab.platform-row">
						{RuntimeConfig.blueskyConnectionsEnabled && (
							<Tooltip text={BLUESKY_PROVIDER_NAME} data-flx="user.linked-accounts-tab.tooltip">
								<button
									type="button"
									className={styles.platformIconButton}
									onClick={() => handleAddConnection(ConnectionTypes.BLUESKY)}
									aria-label={i18n._(ADD_CONNECTION_DESCRIPTOR, {blueskyProviderName: BLUESKY_PROVIDER_NAME})}
									data-flx="user.linked-accounts-tab.platform-icon-button.add-connection"
								>
									<BlueskyIcon size={28} data-flx="user.linked-accounts-tab.bluesky-icon" />
								</button>
							</Tooltip>
						)}
						<Tooltip text={i18n._(DOMAIN_DESCRIPTOR)} data-flx="user.linked-accounts-tab.tooltip--2">
							<button
								type="button"
								className={styles.platformIconButton}
								onClick={() => handleAddConnection(ConnectionTypes.DOMAIN)}
								aria-label={i18n._(ADD_DOMAIN_CONNECTION_DESCRIPTOR)}
								data-flx="user.linked-accounts-tab.platform-icon-button.add-connection--2"
							>
								<GlobeSimpleIcon
									size={remFromPx(28)}
									className={styles.domainIcon}
									data-flx="user.linked-accounts-tab.domain-icon"
								/>
							</button>
						</Tooltip>
					</div>
					{connections.length === 0 ? (
						<div className={styles.emptyState} data-flx="user.linked-accounts-tab.empty-state">
							<StatusSlate
								Icon={UserListIcon}
								title={<Trans>No connections yet</Trans>}
								description={
									<Trans>
										Link your {BLUESKY_PROVIDER_NAME} account or verify domain ownership to display them on your
										profile.
									</Trans>
								}
								data-flx="user.linked-accounts-tab.status-slate"
							/>
						</div>
					) : (
						<DndProvider backend={HTML5Backend} data-flx="user.linked-accounts-tab.dnd-provider">
							<ConnectionDragAutoScroll listRef={connectionsListRef} />
							<div
								ref={connectionsListRef}
								className={styles.connectionsList}
								data-flx="user.linked-accounts-tab.connections-list"
							>
								{connections.map((connection, index) => (
									<ConnectionCard
										key={connection.id}
										connection={connection}
										index={index}
										onDelete={() => handleDelete(connection)}
										onEdit={() => handleEdit(connection)}
										onMoveConnection={handleMoveConnection}
										onDropConnection={handleDropConnection}
										data-flx="user.linked-accounts-tab.connection-card"
									/>
								))}
							</div>
						</DndProvider>
					)}
				</SettingsSection>
			</SettingsTabContent>
		</SettingsTabContainer>
	);
});

export default LinkedAccountsTab;
