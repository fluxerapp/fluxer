import {SettingsTabContainer} from '@app/features/app/components/dialogs/shared/SettingsTabLayout';
import {StatusSlate} from '@app/features/app/components/dialogs/shared/StatusSlate';
import {isKeyboardActivationKey} from '@app/features/input/utils/KeyboardUtils';
import * as PersonaImportCommands from '@app/features/personas/commands/PersonaImports';
import {fetchUserPersonas} from '@app/features/personas/commands/Personas';
import {
	PersonaImportModal,
	type PersonaImportModalProps,
} from '@app/features/personas/components/modals/PersonaImportModal';
import {PERSONA_FILTER_PLACEHOLDER_DESCRIPTOR} from '@app/features/personas/components/popouts/PersonaPickerPopout';
import {Persona} from '@app/features/personas/models/Persona';
import {ChevronRightIcon} from '@app/features/ui/action_menu/ContextMenuIcons';
import {Button} from '@app/features/ui/button/Button';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {modal} from '@app/features/ui/commands/ModalCommands';
import {Avatar} from '@app/features/ui/components/Avatar';
import {Input} from '@app/features/ui/components/form/FormInput';
import MobileLayout from '@app/features/ui/state/MobileLayout';
import {Tooltip} from '@app/features/ui/tooltip/Tooltip';
import type {User} from '@app/features/user/models/User';
import Personas from '@app/features/user/state/Personas';
import Users from '@app/features/user/state/Users';
import {i18n} from '@lingui/core';
import {msg} from '@lingui/core/macro';
import {Trans, useLingui} from '@lingui/react/macro';
import {
	CircleIcon,
	DownloadIcon,
	PlusIcon,
	RadioButtonIcon,
	UserCircleDashedIcon,
	UserCirclePlusIcon,
} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import {runInAction} from 'mobx';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useCallback, useEffect, useMemo, useRef, useState} from 'react';
import {PersonaEditorModal} from '../PersonaEditorModal';
import styles from './PersonasTab.module.css';

const ACTIVATE_PERSONA_DESCRIPTOR = msg({
	message: 'Make persona active',
	comment: 'A tooltip for the button that activates a persona.',
});
const DEACTIVATE_PERSONA_DESCRIPTOR = msg({
	message: 'Make persona not active',
	comment: 'A tooltip for the button that deactivates a persona.',
});
const NO_PERSONAS_DESCRIPTOR = msg({
	message: 'No personas yet',
	comment: 'Slate title that shows in the Personas tab when there are no personas.',
});
const CREATE_A_PERSONA_TO_GET_STARTED_DESCRIPTOR = msg({
	//message: "Create a persona to get started.",
	message: 'My name is Darth Vader. I am an extra-terrestrial from the planet Vulcan!',
	comment: 'Slate message that shows in the Personas tab when there are no personas.',
});
const NO_PERSONAS_FOUND_DESCRIPTOR = msg({
	message: 'No personas found',
	comment: 'Slate title that shows in the Personas tab when there are no personas when filtered.',
});
const TRY_A_DIFFERENT_SEARCH_DESCRIPTOR = msg({
	message: 'Try a different search, or check your spelling.',
	comment: 'Slate message that shows in the Personas tab when there are no personas when filtered.',
});
const ACCEPTED_IMPORT_FORMATS_DESCRIPTOR = msg({
	message:
		'You can import JSON files exported from Pluralkit, Tupperbox, /plu/ral, Fishing Bucket, or anything that exports to a compatible format.',
	comment:
		'A list of supported import formats. The names of each format should remain untranslated since they are proper nouns.',
});

interface PersonaTileProps {
	persona: Persona;
	user: User;
	selected: boolean;
	onSelect?: () => void;
}

const PersonaTile = observer(({persona, user, selected, onSelect}: PersonaTileProps) => {
	const handleOpen = useCallback(async () => {
		const usedPersona = Personas.getPersona(persona.id) ?? persona;
		ModalCommands.push(
			modal(() => (
				// <div>TEST</div>
				// <SwitchGroupSettingsModal
				// 	mode={mode}
				// 	title={title}
				// 	data-flx="user.advanced-settings-tab.advanced-chat-controls.handle-open.switch-group-settings-modal"
				// />
				<PersonaEditorModal
					initialPersona={usedPersona}
					data-flx="user.personas-tab.personas-tab-component.handle-open.persona-editor-modal"
				/>
			)),
		);
	}, [persona.id]);
	const handleKeyDown = useCallback((e: React.KeyboardEvent) => {
		if (isKeyboardActivationKey(e.key)) {
			handleOpen();
			e.preventDefault();
		}
	}, []);
	return (
		<div
			className={styles.personaTile}
			role="button"
			tabIndex={0}
			onClick={handleOpen}
			onKeyDown={handleKeyDown}
			data-flx="user.personas-tab.persona-tile.persona-tile.open"
		>
			<div
				className={styles.personaTileAvatarContainer}
				data-flx="user.personas-tab.persona-tile.persona-tile-avatar-container"
			>
				<Avatar
					user={user}
					size={36}
					//className={className}
					//forceAnimate={isHovering}
					//guildId={guildId}
					personaId={persona?.id}
					personaAvatar={persona?.avatar}
					data-user-id={user!.id}
					//data-guild-id={guildId}
					data-persona-id={persona?.id}
					data-flx="user.personas-tab.personas-tab-component.persona-avatar"
				/>
			</div>
			<div
				className={styles.personaTileDetailsContainer}
				data-flx="user.personas-tab.persona-tile.persona-tile-details-container"
			>
				<span className={styles.personaTileName} data-flx="user.personas-tab.persona-tile.persona-tile-name">
					{persona.internal_name || '<ERROR>'}
				</span>
				<span
					className={styles.personaTileTriggerRow}
					data-flx="user.personas-tab.persona-tile.persona-tile-trigger-row"
				>
					{persona.triggers.map((v) => (
						<span
							className={styles.personaTileTriggerChip}
							data-flx="user.personas-tab.persona-tile.persona-tile-trigger-chip"
						>{`${v.prefix || ''}message${v.suffix || ''}`}</span>
					))}
				</span>
			</div>
			<Tooltip
				text={selected ? i18n._(DEACTIVATE_PERSONA_DESCRIPTOR) : i18n._(ACTIVATE_PERSONA_DESCRIPTOR)}
				data-flx="user.personas-tab.persona-tile.tooltip"
			>
				<Button
					variant="secondary"
					square
					disabled={!onSelect}
					aria-label={selected ? i18n._(DEACTIVATE_PERSONA_DESCRIPTOR) : i18n._(ACTIVATE_PERSONA_DESCRIPTOR)}
					onClick={(e: React.MouseEvent) => {
						e.stopPropagation();
						onSelect?.();
					}}
					icon={
						selected ? (
							<RadioButtonIcon data-flx="user.personas-tab.persona-tile.radio-button-icon" />
						) : (
							<CircleIcon weight="regular" data-flx="user.personas-tab.persona-tile.circle-icon" />
						)
					}
					data-flx="user.personas-tab.persona-tile.button.stop-propagation"
				/>
			</Tooltip>
			<ChevronRightIcon data-flx="user.personas-tab.persona-tile.chevron-right-icon" />
		</div>
	);
});

//const PERSONAS_TAB_ID = 'personas';
const PersonasTabComponent = observer(function PersonasTabComponent() {
	const {i18n} = useLingui();
	const user = useMemo(() => Users.currentUser, []);
	// const [ariaAnnouncement, setAriaAnnouncement] = useState('');
	const [allPersonas, setPersonas] = useState<Array<Persona>>([...Personas.getOwnPersonas()]);
	const [filter, setFilter] = useState('');
	const [selected, updateSelected] = useState(() => Personas.getGlobalActivePersona());
	const [importEpoch, setImportEpoch] = useState(0);

	const personaUpdateCallback = useCallback(() => {
		const newPersonas = Personas.getOwnPersonas();
		if (newPersonas !== allPersonas) {
			setPersonas([...newPersonas]);
			updateSelected(Personas.getGlobalActivePersona());
		}
	}, [user]);
	const personas = useMemo(() => {
		const sources = new Map(allPersonas.map((v) => [v.id, v]));
		if (filter === '') return [...sources.values()];
		const sourceValues = [...sources.values()];
		const termedSources = new Map(
			sourceValues.map((v) => [
				v.id,
				[
					v.internal_name,
					v.display_name,
					v.pronouns,
					...v.triggers.map((t) => `${t.prefix || ''} ${t.suffix || ''}`),
					...v.tags,
				]
					.join(' ')
					.toLowerCase(),
			]),
		);
		const results: Array<Persona> = [];
		for (const [id, value] of termedSources.entries()) {
			if (value.includes(filter.toLowerCase())) {
				results.push(sources.get(id)!);
			}
		}
		return results;
	}, [allPersonas, filter]);
	useEffect(() => {
		runInAction(() => {
			fetchUserPersonas().then((personas) => {
				Personas.cachePersonas(personas);
				setPersonas(personas.map((v) => new Persona(v)));
				updateSelected(Personas.getGlobalActivePersona());
				return Personas.subscribe(personaUpdateCallback);
			});
		});
	}, [user, importEpoch]);

	const isMobile = MobileLayout.isMobileLayout();
	const FilterInput = useCallback(
		({filter, disabled}: {filter: string; disabled?: boolean}) => (
			<Input
				label={<Trans>Filter personas</Trans>}
				placeholder={i18n._(PERSONA_FILTER_PLACEHOLDER_DESCRIPTOR)}
				value={filter}
				disabled={disabled}
				type="text"
				onChange={(e) => setFilter(e.target.value)}
				data-flx="user.personas-tab.filter-input.input.set-filter.text"
			/>
		),
		[setFilter],
	);

	const [isImporting, setImporting] = useState(false);
	//const importButtonRef = useRef<HTMLButtonElement>(null);
	const importInputRef = useRef<HTMLInputElement>(null);
	useEffect(() => {
		const listener = () => setImporting(false);
		importInputRef.current?.addEventListener('cancel', listener);
		return () => {
			importInputRef.current?.removeEventListener('cancel', listener);
		};
	}, [importInputRef.current]);
	const defaultImportState = useMemo(
		() =>
			({
				max: 1,
				value: 0,
				stage: 'UNKNOWN',
			}) as PersonaImportModalProps,
		[],
	);
	const [importState, setImportState] = useState<PersonaImportModalProps>(defaultImportState);

	const onImport = useMemo(() => PersonaImportCommands.processImport, []);

	return (
		<>
			{/* <output
			aria-live="assertive"
			aria-atomic="true"
			className={styles.srOnly}
			data-flx="user.personas-tab.personas-tab-component.sr-only"
		>
			{ariaAnnouncement}
		</output> */}
			<SettingsTabContainer data-flx="user.personas-tab.personas-tab-component.settings-tab-container">
				<p data-flx="user.personas-tab.personas-tab-component.personas-explanation">
					<Trans>Personas are reusable profiles that you can attach to messages.</Trans>
				</p>
				{/* <div className={styles.column}></div> */}
				{/* <Button onClick={() => Personas.personas = {}} leftIcon={<BugIcon />}>Reset</Button> */}

				<div
					className={clsx(styles.buttonRow, isMobile && styles.mobile)}
					data-flx="user.personas-tab.personas-tab-component.button-row"
				>
					{!isMobile && (
						<FilterInput
							filter={filter}
							disabled={!allPersonas}
							data-flx="user.personas-tab.personas-tab-component.filter-input"
						/>
					)}
					<Tooltip
						maxWidth="xl"
						position="top"
						text={i18n._(ACCEPTED_IMPORT_FORMATS_DESCRIPTOR)}
						data-flx="user.personas-tab.personas-tab-component.tooltip"
					>
						<Button
							variant="secondary"
							submitting={isImporting}
							leftIcon={<DownloadIcon data-flx="user.personas-tab.personas-tab-component.download-icon" />}
							onClick={() => {
								if (isImporting) return;
								setImporting(true);
								importInputRef.current?.click();
							}}
							data-flx="user.personas-tab.personas-tab-component.import-personas-button"
						>
							<Trans>Import...</Trans>
						</Button>
					</Tooltip>
					<input
						type="file"
						accept=".json,application/json"
						className={styles.hiddenInput}
						ref={importInputRef}
						onChange={async (e) => {
							setImportState((state) => ({...state, stage: 'PARSING'}));
							const data = await e.target.files?.item(0)?.text();
							if (!data) {
								setImporting(false);
								return;
							}
							setImporting(true);
							void onImport(
								data,
								setImportState,
								() => setImportEpoch((i) => i + 1),
								() => setImporting(false),
							);
						}}
						data-flx="user.personas-tab.personas-tab-component.hidden-input.set-import-state.file"
					/>
					<Button
						variant="primary"
						leftIcon={<PlusIcon data-flx="user.personas-tab.personas-tab-component.plus-icon" />}
						onClick={() => {
							ModalCommands.push(() => (
								<PersonaEditorModal
									initialPersona={null}
									data-flx="user.personas-tab.personas-tab-component.persona-editor-modal"
								/>
							));
						}}
						data-flx="user.personas-tab.personas-tab-component.create-personas-button"
					>
						<Trans>Create Persona</Trans>
					</Button>
				</div>
				{isMobile && allPersonas && (
					<FilterInput filter={filter} data-flx="user.personas-tab.personas-tab-component.filter-input--2" />
				)}
				{(user?.maxPersonas ?? 0) > 0 && (
					<div className={styles.personaLimitRow} data-flx="user.personas-tab.personas-tab-component.persona-limit-row">
						{personas.length}/{user?.maxPersonas}
					</div>
				)}
				{personas.map((v) => (
					<div key={v.id} data-flx="user.personas-tab.personas-tab-component.div">
						<PersonaTile
							persona={v}
							user={user!}
							selected={selected?.id === v.id}
							onSelect={() =>
								runInAction(() => {
									if (selected?.id === v.id) {
										Personas.setGlobalActivePersona('');
									} else {
										Personas.setGlobalActivePersona(v.id);
									}
									updateSelected(Personas.getGlobalActivePersona());
								})
							}
							data-flx="user.personas-tab.personas-tab-component.persona-tile.run-in-action"
						/>
					</div>
				))}
				{personas.length === 0 &&
					(filter ? (
						<StatusSlate
							Icon={UserCircleDashedIcon}
							title={i18n._(NO_PERSONAS_FOUND_DESCRIPTOR)}
							description={i18n._(TRY_A_DIFFERENT_SEARCH_DESCRIPTOR)}
							data-flx="user.personas-tab.personas-tab-component.status-slate"
						/>
					) : (
						<StatusSlate
							Icon={UserCirclePlusIcon}
							title={i18n._(NO_PERSONAS_DESCRIPTOR)}
							description={i18n._(CREATE_A_PERSONA_TO_GET_STARTED_DESCRIPTOR)}
							data-flx="user.personas-tab.personas-tab-component.status-slate--2"
						/>
					))}
			</SettingsTabContainer>
			<PersonaImportModal
				data-flx="user.personas-tab.personas-tab-component.persona-import-modal"
				{...importState}
				onClose={() => setImportState(defaultImportState)}
			/>
		</>
	);
});

export default PersonasTabComponent;
