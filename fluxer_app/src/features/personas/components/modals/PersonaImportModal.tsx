import {GenericErrorModal} from '@app/features/app/components/alerts/GenericErrorModal';
import * as Modal from '@app/features/app/components/dialogs/Modal';
import styles from '@app/features/personas/components/modals/PersonaImportModal.module.css';
import {Button} from '@app/features/ui/button/Button';
import {Checkbox} from '@app/features/ui/checkbox/Checkbox';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {Avatar} from '@app/features/ui/components/Avatar';
import Users from '@app/features/user/state/Users';
import {Progress} from '@base-ui/react/progress';
import type {PersonaCreateRequest as NewPersona} from '@fluxer/schema/src/domains/persona/PersonaSchemas.js';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react';
import {Trans} from '@lingui/react/macro';
import {CheckCircleIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useCallback, useEffect, useMemo, useState} from 'react';

const IMPORT_PROGRESS_DESCRIPTOR = msg({
	message: 'Import progress',
	comment: 'Title of the import progress modal.',
});
const INVALID_FORMAT_DESCRIPTOR = msg({
	message: 'Could not read file',
	comment: 'Title of the import progress modal.',
});
const FETCH_AVATAR_FAILED_DESCRIPTOR = msg({
	message: 'Could not get avatars',
	comment: 'Title of the import progress modal.',
});
const UPLOAD_FAILED_DESCRIPTOR = msg({
	message: 'Could not import personas',
	comment: 'Title of the import progress modal.',
});
const ERROR_GENERIC_DESCRIPTOR = msg({
	message: 'An error occurred',
	comment: 'Title of the import progress modal.',
});

export interface PersonaImportModalProps {
	value: number;
	max: number;
	stage: 'UNKNOWN' | 'PARSING' | 'FETCHING_MEDIA' | 'UPLOADING' | 'DONE' | 'PENDING_FILTER' | 'ERROR';
	error?: {
		code: 'UNKNOWN' | 'INVALID_FORMAT' | 'FETCH_AVATAR_FAILED' | 'UPLOAD_FAILED';
		message: string;
	};
	filterInput?: Array<NewPersona>;
	onFiltered?: (chosen: Array<NewPersona>) => void;
	onClose: () => void;
}

export const PersonaImportModal = observer(({stage = 'UNKNOWN', ...props}: PersonaImportModalProps) => {
	const i18n = useLingui();
	const progressing = ['UNKNOWN', 'FETCHING_MEDIA', 'UPLOADING', 'PARSING'].includes(stage);
	const [waiting, setWaiting] = useState(false);
	const [selectedPersonas, setSelectedPersonas] = useState<Array<number>>([]);
	const user = useMemo(() => Users.currentUser, []);
	const [isVisible, setIsVisible] = useState(false);

	useEffect(() => {
		if (!isVisible && stage !== 'UNKNOWN') {
			setIsVisible(true);
		}
	}, [stage, setIsVisible, isVisible]);

	useEffect(() => {
		if (stage !== 'PENDING_FILTER') {
			setWaiting(false);
		}
	}, [stage, setWaiting]);

	useEffect(() => {
		setSelectedPersonas(props.filterInput?.map((_, i) => i) || []);
	}, [props.filterInput]);

	const onClose = useCallback(() => {
		props.onClose();
		setIsVisible(false);
	}, [props.onClose]);

	if (!isVisible && (stage === 'UNKNOWN' || stage === 'ERROR')) {
		return;
	}

	if (stage === 'ERROR') {
		let errorTitle: string;
		// I fail to see how this is more readable than the nested ternary I had before.
		if (props.error?.code === 'FETCH_AVATAR_FAILED') {
			errorTitle = i18n._(FETCH_AVATAR_FAILED_DESCRIPTOR);
		} else if (props.error?.code === 'UPLOAD_FAILED') {
			errorTitle = i18n._(UPLOAD_FAILED_DESCRIPTOR);
		} else if (props.error?.code === 'INVALID_FORMAT') {
			errorTitle = i18n._(INVALID_FORMAT_DESCRIPTOR);
		} else {
			errorTitle = i18n._(ERROR_GENERIC_DESCRIPTOR);
		}
		const modal = ModalCommands.modal(() => (
			<GenericErrorModal
				title={errorTitle}
				message={props.error?.message || i18n._(ERROR_GENERIC_DESCRIPTOR)}
				data-flx="personas.persona-import-modal.modal.generic-error-modal"
			/>
		));
		setIsVisible(false);
		ModalCommands.push(modal);
		return;
	}

	return (
		<Modal.Root size="small" onClose={onClose} data-flx="personas.persona-import-modal.modal-root">
			<Modal.ScreenReaderLabel
				text={i18n._(IMPORT_PROGRESS_DESCRIPTOR)}
				data-flx="personas.persona-import-modal.modal-screen-reader-label"
			/>
			<Modal.Content data-flx="personas.persona-import-modal.modal-content">
				{progressing && (
					<Progress.Root
						data-flx="this-is-my-progress-bar"
						className={styles.progressBar!}
						max={props.max}
						value={stage === 'FETCHING_MEDIA' ? props.value : null}
					>
						<Progress.Label
							className={styles.progressBarLabel}
							data-flx="personas.persona-import-modal.progress-bar-label"
						>
							{(
								{
									PARSING: <Trans>Reading file...</Trans>,
									FETCHING_MEDIA: <Trans>Preparing avatars and banners...</Trans>,
									UPLOADING: <Trans>Uploading to server...</Trans>,
								} as Record<PersonaImportModalProps['stage'], React.ReactNode>
							)[stage] || '...'}
						</Progress.Label>
						<Progress.Track
							className={styles.progressBarTrack}
							data-flx="personas.persona-import-modal.progress-bar-track"
						>
							<Progress.Indicator
								className={styles.progressBarIndicator}
								data-flx="personas.persona-import-modal.progress-bar-indicator"
							/>
						</Progress.Track>
					</Progress.Root>
				)}
				{stage === 'PENDING_FILTER' && props.filterInput && (
					<div className={styles.filterRoot} data-flx="personas.persona-import-modal.filter-root">
						<div className={styles.filterHeading} data-flx="personas.persona-import-modal.filter-heading">
							<h3 data-flx="personas.persona-import-modal.h3">
								<Trans>Select personas to import</Trans>
							</h3>
							<div className={styles.filterButtonRow} data-flx="personas.persona-import-modal.filter-button-row">
								<Button
									variant="secondary"
									onClick={() => {
										setSelectedPersonas([]);
									}}
									data-flx="personas.persona-import-modal.button.set-selected-personas"
								>
									<Trans>Deselect all</Trans>
								</Button>
								<Button
									variant="primary"
									onClick={() => {
										setSelectedPersonas(props.filterInput?.map((_, i) => i) || []);
									}}
									data-flx="personas.persona-import-modal.button.set-selected-personas--2"
								>
									<Trans>Select all</Trans>
								</Button>
							</div>
						</div>
						<div className={styles.filterItems} data-flx="personas.persona-import-modal.filter-items">
							{props.filterInput.map((persona, i) => (
								<Checkbox
									className={styles.filterItemCheckbox}
									checked={selectedPersonas.includes(i)}
									onChange={(newChecked) => {
										if (newChecked) setSelectedPersonas((items) => [...items, i]);
										else setSelectedPersonas((items) => items.filter((v) => v !== i));
									}}
									data-flx="personas.persona-import-modal.filter-item-checkbox"
								>
									<div className={styles.filterItem} data-flx="personas.persona-import-modal.filter-item">
										<Avatar
											user={user!}
											size={36}
											avatarUrl={persona.avatar}
											hoverAvatarUrl={persona.avatar}
											data-flx="personas.persona-import-modal.avatar"
										/>
										<div data-flx="personas.persona-import-modal.div">{persona.internal_name}</div>
									</div>
								</Checkbox>
							))}
						</div>
					</div>
				)}
				{stage === 'DONE' && (
					<div className={styles.doneRow} data-flx="personas.persona-import-modal.done-row">
						<CheckCircleIcon weight="regular" size={32} data-flx="personas.persona-import-modal.check-circle-icon" />
						<h3 data-flx="personas.persona-import-modal.h3--2">
							<Trans>{props.value} personas imported successfully</Trans>
						</h3>
					</div>
				)}
			</Modal.Content>
			{stage === 'DONE' && (
				<Modal.Footer data-flx="personas.persona-import-modal.modal-footer">
					<Button onClick={onClose} data-flx="personas.persona-import-modal.button.close">
						<Trans>OK</Trans>
					</Button>
				</Modal.Footer>
			)}
			{stage === 'PENDING_FILTER' && (
				<Modal.Footer data-flx="personas.persona-import-modal.modal-footer--2">
					<Button
						disabled={!props.onFiltered && !props.filterInput}
						submitting={waiting}
						onClick={() => {
							setWaiting(true);
							props.onFiltered!(props.filterInput?.filter((_, i) => selectedPersonas.includes(i)) ?? []);
						}}
						data-flx="personas.persona-import-modal.button.set-waiting"
					>
						<Trans>Import</Trans>
					</Button>
				</Modal.Footer>
			)}
		</Modal.Root>
	);
});
