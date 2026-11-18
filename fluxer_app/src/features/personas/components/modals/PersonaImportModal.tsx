import * as Modal from '@app/features/app/components/dialogs/Modal';
import { Progress } from '@base-ui/react/progress';
import { msg } from '@lingui/core/macro';
import { useLingui } from '@lingui/react';
import { Trans } from '@lingui/react/macro';
import { observer } from "mobx-react-lite";
import type React from 'react';
import styles from '@app/features/personas/components/modals/PersonaImportModal.module.css';
import { GenericErrorModal } from '@app/features/app/components/alerts/GenericErrorModal';
import { Button } from '@app/features/ui/button/Button';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import type { Persona } from '../../models/Persona';
import { useCallback, useEffect, useMemo, useState } from 'react';
import { Checkbox } from '@app/features/ui/checkbox/Checkbox';
import { Avatar } from '@app/features/ui/components/Avatar';
import Users from '@app/features/user/state/Users';
import type { PersonaCreateRequest as NewPersona } from "@fluxer/schema/src/domains/persona/PersonaSchemas.js";
import { CheckCircleIcon } from '@phosphor-icons/react';

const IMPORT_PROGRESS_DESCRIPTOR = msg({
	message: "Import progress",
	comment: "Title of the import progress modal."
});
const INVALID_FORMAT_DESCRIPTOR = msg({
	message: "Could not read file",
	comment: "Title of the import progress modal."
});
const FETCH_AVATAR_FAILED_DESCRIPTOR = msg({
	message: "Could not get avatars",
	comment: "Title of the import progress modal."
});
const UPLOAD_FAILED_DESCRIPTOR = msg({
	message: "Could not import personas",
	comment: "Title of the import progress modal."
});
const ERROR_GENERIC_DESCRIPTOR = msg({
	message: "An error occurred",
	comment: "Title of the import progress modal."
});

export interface PersonaImportModalProps {
	value: number,
	max: number,
	stage: "UNKNOWN" | "PARSING" | "FETCHING_MEDIA" | "UPLOADING" | "DONE" | "PENDING_FILTER" | "ERROR",
	error?: {
		code: "UNKNOWN" | "INVALID_FORMAT" | "FETCH_AVATAR_FAILED" | "UPLOAD_FAILED",
		message: string
	},
	filterInput?: Array<NewPersona>,
	onFiltered?: (chosen: Array<NewPersona>) => void,
	onClose: () => void,
}

export type PersonaImportModalError = PersonaImportModalProps["error"];

export const PersonaImportModal = observer(({stage = "UNKNOWN", ...props}: PersonaImportModalProps) => {
	const i18n = useLingui();
	const progressing = ["UNKNOWN", "FETCHING_MEDIA", "UPLOADING", "PARSING"].includes(stage);
	const [waiting, setWaiting] = useState(false);
	const [selectedPersonas, setSelectedPersonas] = useState<Array<number>>([]);
	const user = useMemo(() => Users.currentUser, []);
	const [isVisible, setIsVisible] = useState(false);

	useEffect(() => {
		if (!isVisible && stage !== "UNKNOWN") {
			setIsVisible(true);
		}
	}, [stage, setIsVisible, isVisible]);

	useEffect(() => {
		if (stage !== "PENDING_FILTER") {
			setWaiting(false);
		}
	}, [stage, setWaiting])

	useEffect(() => {
		setSelectedPersonas((props.filterInput?.map((_, i) => i) || []));
	}, [props.filterInput]);

	const onClose = useCallback(() => {
		props.onClose();
		setIsVisible(false);
	}, [props.onClose]);

	if (!isVisible && (stage === "UNKNOWN" || stage === "ERROR")) {
		return;
	}

	if (stage === "ERROR") {
		const modal = ModalCommands.modal(() => <GenericErrorModal
			title={props.error?.code === "FETCH_AVATAR_FAILED" ? i18n._(FETCH_AVATAR_FAILED_DESCRIPTOR)
				: props.error?.code === "UPLOAD_FAILED" ? i18n._(UPLOAD_FAILED_DESCRIPTOR)
				: props.error?.code === "INVALID_FORMAT" ? i18n._(INVALID_FORMAT_DESCRIPTOR)
				: i18n._(ERROR_GENERIC_DESCRIPTOR) //
			}
			message={props.error?.message || i18n._(ERROR_GENERIC_DESCRIPTOR)}
		/>);
		setIsVisible(false);
		ModalCommands.push(modal);
		return;
	}

	return <Modal.Root size="small" onClose={onClose}>
		<Modal.ScreenReaderLabel text={i18n._(IMPORT_PROGRESS_DESCRIPTOR)} />
		<Modal.Content>
			{progressing && <Progress.Root data-flx="this-is-my-progress-bar" className={styles.progressBar!} max={props.max} value={stage === "FETCHING_MEDIA" ? props.value : null}>
				<Progress.Label className={styles.progressBarLabel}>{({
					"PARSING": <Trans>Reading file...</Trans>,
					"FETCHING_MEDIA": <Trans>Preparing avatars and banners...</Trans>,
					"UPLOADING": <Trans>Uploading to server...</Trans>,
				} as Record<PersonaImportModalProps["stage"], React.ReactNode>)[stage] || "..."}</Progress.Label>
				<Progress.Track className={styles.progressBarTrack}>
					<Progress.Indicator className={styles.progressBarIndicator} />
				</Progress.Track>
			</Progress.Root>}
			{stage === "PENDING_FILTER" && props.filterInput && <div className={styles.filterRoot}>
				<div className={styles.filterHeading}>
					<h3><Trans>Select personas to import</Trans></h3>
					<div className={styles.filterButtonRow}>
						<Button
							variant="secondary"
							onClick={() => {
								setSelectedPersonas([]);
							}}
						><Trans>Deselect all</Trans></Button>
						<Button
							variant="primary"
							onClick={() => {
								setSelectedPersonas(props.filterInput?.map((_, i) => i) || []);
							}}
						><Trans>Select all</Trans></Button>
					</div>
				</div>
				<div className={styles.filterItems}>{props.filterInput.map((persona, i) =>
					<Checkbox
					  className={styles.filterItemCheckbox}
						checked={selectedPersonas.includes(i)}
						onChange={(newChecked) => {
							if (newChecked) setSelectedPersonas((items) => [...items, i]);
							else setSelectedPersonas((items) => items.filter((v) => v !== i))
						}}
					>
						<div className={styles.filterItem}>
							<Avatar user={user!} size={36} avatarUrl={persona.avatar} hoverAvatarUrl={persona.avatar} />
							<div>{persona.internal_name}</div>
						</div>
					</Checkbox>
				)}</div>
			</div>}
			{stage === "DONE" && <div className={styles.doneRow}>
				<CheckCircleIcon weight="regular" size={32} />
				<h3><Trans>{props.value} personas imported successfully</Trans></h3>
			</div>}
		</Modal.Content>
		{stage === "DONE" && <Modal.Footer>
			<Button
				onClick={onClose}
			><Trans>OK</Trans></Button>
		</Modal.Footer>}
		{stage === "PENDING_FILTER" && <Modal.Footer>
			<Button
				disabled={!props.onFiltered && !props.filterInput}
				submitting={waiting}
				onClick={() => {
					setWaiting(true);
					props.onFiltered!(props.filterInput?.filter((_, i) => selectedPersonas.includes(i)) ?? []);
				}}
			><Trans>Import</Trans></Button>
		</Modal.Footer>}
	</Modal.Root>;
});
