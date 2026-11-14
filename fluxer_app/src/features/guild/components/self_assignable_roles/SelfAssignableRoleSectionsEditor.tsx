// SPDX-License-Identifier: AGPL-3.0-or-later

import * as GuildCommands from '@app/features/guild/commands/GuildCommands';
import {
	openSelfAssignableRolePickerMenu,
	SelfAssignableRolePickerSheet,
} from '@app/features/guild/components/self_assignable_roles/SelfAssignableRolePicker';
import styles from '@app/features/guild/components/self_assignable_roles/SelfAssignableRoles.module.css';
import {
	RoleChipLabel,
	SelfAssignableRolesEmptyState,
} from '@app/features/guild/components/self_assignable_roles/SelfAssignableRolesShared';
import {
	type SelfAssignableRoleReorderHandlers,
	useSelfAssignableRoleListDrop,
	useSelfAssignableRoleReorder,
} from '@app/features/guild/components/self_assignable_roles/useSelfAssignableRoleReorder';
import type {Guild, GuildSelfAssignableRole} from '@app/features/guild/models/Guild';
import type {GuildRole} from '@app/features/guild/models/GuildRole';
import {
	getAvailableSelfAssignableRoleSections,
	resolveSelfAssignableRoles,
} from '@app/features/guild/utils/SelfAssignableRoles';
import {
	CANCEL_DESCRIPTOR,
	DESCRIPTION_DESCRIPTOR,
	ROLES_DESCRIPTOR,
} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {remFromPx} from '@app/features/theme/layout/RemFromPx';
import {Button} from '@app/features/ui/button/Button';
import {Input, Textarea} from '@app/features/ui/components/form/FormInput';
import formStyles from '@app/features/ui/components/form/FormInput.module.css';
import FocusRing from '@app/features/ui/focus_ring/FocusRing';
import MobileLayout from '@app/features/ui/state/MobileLayout';
import {StepSliderRow} from '@app/features/user/components/modals/tabs/appearance_tab/StepSliderRow';
import {extractErrorMessage} from '@app/lib/forms';
import {moveKey} from '@app/lib/list/MoveKey';
import {
	MAX_SELF_ASSIGNABLE_ROLE_SECTIONS,
	MAX_SELF_ASSIGNABLE_ROLES_PER_SECTION,
	SELF_ASSIGNABLE_ROLE_DESCRIPTION_MAX_LENGTH,
	SELF_ASSIGNABLE_ROLE_SECTION_CONTENT_MAX_LENGTH,
} from '@fluxer/constants/src/GuildConstants';
import type {I18n} from '@lingui/core';
import {msg} from '@lingui/core/macro';
import {Trans, useLingui} from '@lingui/react/macro';
import {ArrowDownIcon, ArrowUpIcon, DotsSixVerticalIcon, PlusIcon, TrashIcon, XIcon} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import isEqual from 'lodash/isEqual';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useCallback, useMemo, useRef, useState} from 'react';

const SECTION_NUMBER_DESCRIPTOR = msg({
	message: 'Section {sectionNumber}',
	comment:
		'Heading of one section card in the self-assignable role section editor. {sectionNumber} is the 1-based position of the section.',
});
const MOVE_SECTION_UP_DESCRIPTOR = msg({
	message: 'Move section up',
	comment: 'Accessible label of the button that moves a section one place up in the self-assignable role editor.',
});
const MOVE_SECTION_DOWN_DESCRIPTOR = msg({
	message: 'Move section down',
	comment: 'Accessible label of the button that moves a section one place down in the self-assignable role editor.',
});
const DELETE_SECTION_DESCRIPTOR = msg({
	message: 'Delete section',
	comment: 'Accessible label of the button that removes a section in the self-assignable role editor.',
});
const SECTION_TEXT_PLACEHOLDER_DESCRIPTOR = msg({
	message: 'Tell members what these roles are for',
	comment: 'Placeholder of the text box in a self-assignable role section in the editor.',
});
const REMOVE_ROLE_FROM_SECTION_DESCRIPTOR = msg({
	message: 'Remove {roleName} from this section',
	comment:
		'Accessible label of the button that takes a role out of a self-assignable role section. {roleName} is the role name.',
});
const MOVE_ROLE_DESCRIPTOR = msg({
	message: 'Move {roleName}',
	comment:
		'Accessible label of the drag handle that moves a role up or down in a self-assignable role section. {roleName} is the role name.',
});
const ROLE_DESCRIPTION_LABEL_DESCRIPTOR = msg({
	message: 'Description for {roleName}',
	comment:
		'Accessible label of the text box next to a role in the self-assignable role section editor. {roleName} is the role name.',
});
const ROLE_DESCRIPTION_PLACEHOLDER_DESCRIPTOR = msg({
	message: 'Description (optional)',
	comment:
		'Placeholder of the text box next to a role in the self-assignable role section editor. Members see the text with the role, so they know what they are choosing.',
});
const ADD_AT_LEAST_ONE_ROLE_DESCRIPTOR = msg({
	message: 'Add at least one role.',
	comment: 'Error under a self-assignable role section that has no roles. The section cannot be saved like this.',
});
const ROLE_LIMIT_LABEL_DESCRIPTOR = msg({
	message: 'Members may choose',
	comment:
		'Label of the slider in a self-assignable role section that sets how many of its roles each member can choose. The chosen value shows next to it, for example "Only one role".',
});
const ROLE_LIMIT_ANY_DESCRIPTOR = msg({
	message: 'Any number of roles',
	comment:
		'Value of the self-assignable role section slider at its lowest point, meaning no limit on how many of its roles a member can choose.',
});
const ROLE_LIMIT_ONE_DESCRIPTOR = msg({
	message: 'Only one role',
	comment:
		'Value of the self-assignable role section slider that limits each member to one role from the section. Choosing another role swaps it.',
});
const ROLE_LIMIT_UP_TO_DESCRIPTOR = msg({
	message: 'Up to {count, plural, one {# role} other {# roles}}',
	comment:
		'Value of the self-assignable role section slider that limits how many of its roles each member can choose. {count} is the limit and is at least 2.',
});
const REQUIRED_ROLES_LABEL_DESCRIPTOR = msg({
	message: 'Only show to members with',
	comment:
		'Label above the roles a member needs one of to see a self-assignable role section and choose its roles. Leave it empty to show the section to everyone.',
});
const EVERYONE_CAN_SEE_DESCRIPTOR = msg({
	message: 'Everyone can see this section.',
	comment: 'Shown under the required roles of a self-assignable role section in the editor when none are set.',
});
const STOP_REQUIRING_ROLE_DESCRIPTOR = msg({
	message: 'Stop requiring {roleName}',
	comment:
		'Accessible label of the button that removes a role from the roles members need to see a self-assignable role section. {roleName} is the role name.',
});
const UNKNOWN_ROLE_DESCRIPTOR = msg({
	message: 'Unknown role',
	comment:
		'Label in the self-assignable role section editor for a required role that was deleted. Nobody has it, so the section stays hidden until it is removed.',
});
const NO_SECTIONS_YET_DESCRIPTOR = msg({
	message: 'No sections yet. Add one to let members pick roles for themselves.',
	comment: 'Shown in the self-assignable role editor when there are no sections.',
});

function roleLimitStops(roleCount: number, current: number | null): Array<number> {
	const max = Math.max(1, roleCount - 1, current ?? 0);
	return Array.from({length: max + 1}, (_, index) => index);
}

function formatRoleLimit(i18n: I18n, limit: number): string {
	if (limit === 0) return i18n._(ROLE_LIMIT_ANY_DESCRIPTOR);
	if (limit === 1) return i18n._(ROLE_LIMIT_ONE_DESCRIPTOR);
	return i18n._(ROLE_LIMIT_UP_TO_DESCRIPTOR, {count: limit});
}

interface DraftSection {
	readonly key: string;
	readonly id: string | null;
	readonly content: string;
	readonly roles: ReadonlyArray<GuildSelfAssignableRole>;
	readonly maxRoles: number | null;
	readonly requiredRoleIds: ReadonlyArray<string>;
}

function createDraft(guild: Guild): Array<DraftSection> {
	return getAvailableSelfAssignableRoleSections(guild).map(({section, roles}) => ({
		key: section.id,
		id: section.id,
		content: section.content,
		roles: roles.map(({role, description}) => ({id: role.id, description})),
		maxRoles: section.maxRoles,
		requiredRoleIds: section.requiredRoleIds,
	}));
}

function toSectionsPayload(draft: ReadonlyArray<DraftSection>): Array<GuildCommands.SelfAssignableRoleSectionInput> {
	return draft.map((section) => ({
		...(section.id ? {id: section.id} : {}),
		content: section.content.trim(),
		roles: section.roles.map((role) => ({id: role.id, description: role.description.trim()})),
		max_roles: section.maxRoles,
		required_role_ids: [...section.requiredRoleIds],
	}));
}

interface EditorRoleRowProps extends SelfAssignableRoleReorderHandlers {
	readonly sectionKey: string;
	readonly role: GuildRole;
	readonly description: string;
	readonly order: ReadonlyArray<string>;
	readonly reorderable: boolean;
	readonly onDescriptionChange: (roleId: string, description: string) => void;
	readonly onRemove: (roleId: string) => void;
}

function EditorRoleRow({
	sectionKey,
	role,
	description,
	order,
	reorderable,
	onDescriptionChange,
	onRemove,
	onDragStart,
	onMoveRole,
	onDragEnd,
}: EditorRoleRowProps): React.ReactElement {
	const {i18n} = useLingui();
	const {isDragging, rowRef, handleRef} = useSelfAssignableRoleReorder({
		sectionKey,
		roleId: role.id,
		order,
		enabled: reorderable,
		onDragStart,
		onMoveRole,
		onDragEnd,
	});
	return (
		<div
			ref={rowRef}
			className={clsx(styles.editorRoleRow, isDragging && styles.editorRoleRowDragging)}
			data-flx="guild.self-assignable-roles.self-assignable-role-sections-editor.editor-role-row.editor-role-row"
		>
			<FocusRing
				offset={-2}
				data-flx="guild.self-assignable-roles.self-assignable-role-sections-editor.editor-role-row.focus-ring"
			>
				<button
					ref={handleRef}
					type="button"
					className={styles.editorRoleHandle}
					aria-label={i18n._(MOVE_ROLE_DESCRIPTOR, {roleName: role.name})}
					disabled={!reorderable}
					data-flx="guild.self-assignable-roles.self-assignable-role-sections-editor.editor-role-row.editor-role-handle.button"
				>
					<DotsSixVerticalIcon
						size={remFromPx(16)}
						weight="bold"
						data-flx="guild.self-assignable-roles.self-assignable-role-sections-editor.editor-role-row.dots-six-vertical-icon"
					/>
				</button>
			</FocusRing>
			<span
				className={styles.editorRoleName}
				data-flx="guild.self-assignable-roles.self-assignable-role-sections-editor.editor-role-row.editor-role-name"
			>
				<RoleChipLabel
					role={role}
					name={role.name}
					data-flx="guild.self-assignable-roles.self-assignable-role-sections-editor.editor-role-row.role-chip-label"
				/>
			</span>
			<div
				className={styles.editorRoleDescription}
				data-flx="guild.self-assignable-roles.self-assignable-role-sections-editor.editor-role-row.editor-role-description"
			>
				<Input
					value={description}
					dir="auto"
					placeholder={i18n._(ROLE_DESCRIPTION_PLACEHOLDER_DESCRIPTOR)}
					aria-label={i18n._(ROLE_DESCRIPTION_LABEL_DESCRIPTOR, {roleName: role.name})}
					maxLength={SELF_ASSIGNABLE_ROLE_DESCRIPTION_MAX_LENGTH}
					onChange={(event) => onDescriptionChange(role.id, event.target.value)}
					data-flx="guild.self-assignable-roles.self-assignable-role-sections-editor.editor-role-row.input.description-change"
				/>
			</div>
			<FocusRing
				offset={-2}
				data-flx="guild.self-assignable-roles.self-assignable-role-sections-editor.editor-role-row.focus-ring--2"
			>
				<button
					type="button"
					className={clsx(styles.editorRolePillRemove, styles.editorRoleRemove)}
					aria-label={i18n._(REMOVE_ROLE_FROM_SECTION_DESCRIPTOR, {roleName: role.name})}
					onClick={() => onRemove(role.id)}
					data-flx="guild.self-assignable-roles.self-assignable-role-sections-editor.editor-role-row.editor-role.remove.button"
				>
					<XIcon
						size={remFromPx(12)}
						weight="bold"
						data-flx="guild.self-assignable-roles.self-assignable-role-sections-editor.editor-role-row.x-icon"
					/>
				</button>
			</FocusRing>
		</div>
	);
}

const EditorRoleList: React.FC<{readonly sectionKey: string; readonly children: React.ReactNode}> = ({
	sectionKey,
	children,
}) => {
	const dropRef = useSelfAssignableRoleListDrop(sectionKey);
	return (
		<div
			ref={dropRef}
			className={styles.editorRoleRows}
			data-flx="guild.self-assignable-roles.self-assignable-role-sections-editor.editor-role-list"
		>
			{children}
		</div>
	);
};

interface RequiredRolesFieldProps {
	readonly guild: Guild;
	readonly roleIds: ReadonlyArray<string>;
	readonly onAdd: (event: React.MouseEvent) => void;
	readonly onRemove: (roleId: string) => void;
}

const RequiredRolesField: React.FC<RequiredRolesFieldProps> = observer(function RequiredRolesField({
	guild,
	roleIds,
	onAdd,
	onRemove,
}) {
	const {i18n} = useLingui();
	return (
		<div
			className={styles.editorRoles}
			data-flx="guild.self-assignable-roles.self-assignable-role-sections-editor.required-roles-field.editor-roles"
		>
			<p
				className={formStyles.label}
				data-flx="guild.self-assignable-roles.self-assignable-role-sections-editor.required-roles-field.editor-roles-label"
			>
				{i18n._(REQUIRED_ROLES_LABEL_DESCRIPTOR)}
			</p>
			<div
				className={styles.editorRoleList}
				data-flx="guild.self-assignable-roles.self-assignable-role-sections-editor.required-roles-field.editor-role-list"
			>
				{roleIds.map((roleId) => {
					const role = guild.roles[roleId];
					const name = role?.name ?? i18n._(UNKNOWN_ROLE_DESCRIPTOR);
					return (
						<span
							key={roleId}
							className={styles.editorRolePill}
							data-flx="guild.self-assignable-roles.self-assignable-role-sections-editor.required-roles-field.editor-role-pill"
						>
							<RoleChipLabel
								role={role ?? null}
								name={name}
								data-flx="guild.self-assignable-roles.self-assignable-role-sections-editor.required-roles-field.role-chip-label"
							/>
							<FocusRing
								offset={-2}
								data-flx="guild.self-assignable-roles.self-assignable-role-sections-editor.required-roles-field.focus-ring"
							>
								<button
									type="button"
									className={styles.editorRolePillRemove}
									aria-label={i18n._(STOP_REQUIRING_ROLE_DESCRIPTOR, {roleName: name})}
									onClick={() => onRemove(roleId)}
									data-flx="guild.self-assignable-roles.self-assignable-role-sections-editor.required-roles-field.editor-role-pill-remove.button"
								>
									<XIcon
										size={remFromPx(12)}
										weight="bold"
										data-flx="guild.self-assignable-roles.self-assignable-role-sections-editor.required-roles-field.x-icon"
									/>
								</button>
							</FocusRing>
						</span>
					);
				})}
				<FocusRing
					offset={-2}
					data-flx="guild.self-assignable-roles.self-assignable-role-sections-editor.required-roles-field.focus-ring--2"
				>
					<button
						type="button"
						className={styles.addRoleButton}
						disabled={roleIds.length >= MAX_SELF_ASSIGNABLE_ROLES_PER_SECTION}
						onClick={onAdd}
						data-flx="guild.self-assignable-roles.self-assignable-role-sections-editor.required-roles-field.add-role-button"
					>
						<PlusIcon
							size={remFromPx(14)}
							weight="bold"
							data-flx="guild.self-assignable-roles.self-assignable-role-sections-editor.required-roles-field.plus-icon"
						/>
						<span data-flx="guild.self-assignable-roles.self-assignable-role-sections-editor.required-roles-field.span">
							<Trans>Add role</Trans>
						</span>
					</button>
				</FocusRing>
			</div>
			{roleIds.length === 0 && (
				<p
					className={styles.sectionHint}
					data-flx="guild.self-assignable-roles.self-assignable-role-sections-editor.required-roles-field.section-hint"
				>
					{i18n._(EVERYONE_CAN_SEE_DESCRIPTOR)}
				</p>
			)}
		</div>
	);
});

interface SelfAssignableRoleSectionsEditorProps {
	readonly guild: Guild;
	readonly startWithNewSection: boolean;
	readonly onClose: () => void;
}

export const SelfAssignableRoleSectionsEditor: React.FC<SelfAssignableRoleSectionsEditorProps> = observer(
	function SelfAssignableRoleSectionsEditor({guild, startWithNewSection, onClose}) {
		const {i18n} = useLingui();
		const isMobile = MobileLayout.enabled;
		const nextKeyRef = useRef(0);
		const createSection = useCallback((): DraftSection => {
			nextKeyRef.current += 1;
			return {
				key: `new-${nextKeyRef.current}`,
				id: null,
				content: '',
				roles: [],
				maxRoles: null,
				requiredRoleIds: [],
			};
		}, []);
		const [initialDraft] = useState(() => createDraft(guild));
		const [draft, setDraft] = useState<Array<DraftSection>>(() =>
			startWithNewSection && initialDraft.length === 0 ? [createSection()] : initialDraft,
		);
		const [saving, setSaving] = useState(false);
		const [saveError, setSaveError] = useState<string | null>(null);
		const [picker, setPicker] = useState<{readonly key: string; readonly required: boolean} | null>(null);
		const isDirty = !isEqual(toSectionsPayload(draft), toSectionsPayload(initialDraft));
		const hasInvalidSection = draft.some(
			(section) =>
				section.roles.length === 0 || section.content.length > SELF_ASSIGNABLE_ROLE_SECTION_CONTENT_MAX_LENGTH,
		);
		const updateSection = useCallback((key: string, update: (section: DraftSection) => DraftSection) => {
			setDraft((current) => current.map((section) => (section.key === key ? update(section) : section)));
		}, []);
		const setRoleInSection = useCallback(
			(key: string, roleId: string, checked: boolean) => {
				updateSection(key, (section) => {
					if (checked) {
						if (section.roles.some((role) => role.id === roleId)) return section;
						if (section.roles.length >= MAX_SELF_ASSIGNABLE_ROLES_PER_SECTION) return section;
						return {...section, roles: [...section.roles, {id: roleId, description: ''}]};
					}
					return {...section, roles: section.roles.filter((role) => role.id !== roleId)};
				});
			},
			[updateSection],
		);
		const setRoleDescription = useCallback(
			(key: string, roleId: string, description: string) => {
				updateSection(key, (section) => ({
					...section,
					roles: section.roles.map((role) => (role.id === roleId ? {...role, description} : role)),
				}));
			},
			[updateSection],
		);
		const setRequiredRoleInSection = useCallback(
			(key: string, roleId: string, checked: boolean) => {
				updateSection(key, (section) => {
					if (!checked) {
						return {...section, requiredRoleIds: section.requiredRoleIds.filter((id) => id !== roleId)};
					}
					if (section.requiredRoleIds.includes(roleId)) return section;
					if (section.requiredRoleIds.length >= MAX_SELF_ASSIGNABLE_ROLES_PER_SECTION) return section;
					return {...section, requiredRoleIds: [...section.requiredRoleIds, roleId]};
				});
			},
			[updateSection],
		);
		const moveSection = useCallback((index: number, offset: number) => {
			setDraft((current) => {
				const target = index + offset;
				if (target < 0 || target >= current.length) return current;
				const byKey = new Map(current.map((section) => [section.key, section]));
				return moveKey([...byKey.keys()], current[index]!.key, target).map((key) => byKey.get(key)!);
			});
		}, []);
		const draftRef = useRef(draft);
		draftRef.current = draft;
		const dragSnapshotRef = useRef<{
			readonly sectionKey: string;
			readonly roles: ReadonlyArray<GuildSelfAssignableRole>;
		} | null>(null);
		const handleRoleDragStart = useCallback((sectionKey: string) => {
			const section = draftRef.current.find((candidate) => candidate.key === sectionKey);
			dragSnapshotRef.current = section ? {sectionKey, roles: section.roles} : null;
		}, []);
		const handleMoveRole = useCallback(
			(sectionKey: string, roleId: string, index: number) => {
				updateSection(sectionKey, (section) => {
					const byId = new Map(section.roles.map((role) => [role.id, role]));
					const order = moveKey(
						section.roles.map((role) => role.id),
						roleId,
						index,
					);
					return {...section, roles: order.map((id) => byId.get(id)!)};
				});
			},
			[updateSection],
		);
		const handleRoleDragEnd = useCallback(
			(didDrop: boolean) => {
				const snapshot = dragSnapshotRef.current;
				dragSnapshotRef.current = null;
				if (!didDrop && snapshot) {
					updateSection(snapshot.sectionKey, (section) => ({...section, roles: snapshot.roles}));
				}
			},
			[updateSection],
		);
		const getUnavailableRoleIds = useCallback(
			(key: string): ReadonlySet<string> =>
				new Set(
					draft.filter((section) => section.key !== key).flatMap((section) => section.roles.map((role) => role.id)),
				),
			[draft],
		);
		const pickerSection = useMemo(() => draft.find((section) => section.key === picker?.key) ?? null, [draft, picker]);
		const pickerRoleIds = useMemo(() => pickerSection?.roles.map((role) => role.id) ?? [], [pickerSection]);
		const handleSave = async () => {
			setSaving(true);
			setSaveError(null);
			try {
				await GuildCommands.updateSelfAssignableRoleSections(guild.id, toSectionsPayload(draft));
				onClose();
			} catch (error) {
				setSaveError(extractErrorMessage(i18n, error));
			} finally {
				setSaving(false);
			}
		};
		const handleAddRole = (event: React.MouseEvent, section: DraftSection) => {
			if (isMobile) {
				setPicker({key: section.key, required: false});
				return;
			}
			openSelfAssignableRolePickerMenu(event, {
				guildId: guild.id,
				unavailableRoleIds: getUnavailableRoleIds(section.key),
				selectedRoleIds: section.roles.map((role) => role.id),
				onChange: (roleId, checked) => setRoleInSection(section.key, roleId, checked),
			});
		};
		const handleAddRequiredRole = (event: React.MouseEvent, section: DraftSection) => {
			if (isMobile) {
				setPicker({key: section.key, required: true});
				return;
			}
			openSelfAssignableRolePickerMenu(event, {
				guildId: guild.id,
				unavailableRoleIds: new Set(),
				allowUnmanageable: true,
				selectedRoleIds: section.requiredRoleIds,
				onChange: (roleId, checked) => setRequiredRoleInSection(section.key, roleId, checked),
			});
		};
		return (
			<>
				{draft.length === 0 && (
					<SelfAssignableRolesEmptyState
						message={i18n._(NO_SECTIONS_YET_DESCRIPTOR)}
						data-flx="guild.self-assignable-roles.self-assignable-role-sections-editor.self-assignable-roles-empty-state"
					/>
				)}
				{draft.map((section, index) => {
					const roleOrder = section.roles.map((role) => role.id);
					const rows = resolveSelfAssignableRoles(guild, section.roles);
					return (
						<section
							key={section.key}
							className={styles.section}
							data-flx="guild.self-assignable-roles.self-assignable-role-sections-editor.section"
						>
							<div
								className={styles.editorSectionHeader}
								data-flx="guild.self-assignable-roles.self-assignable-role-sections-editor.editor-section-header"
							>
								<h3
									className={styles.editorSectionTitle}
									data-flx="guild.self-assignable-roles.self-assignable-role-sections-editor.editor-section-title"
								>
									{i18n._(SECTION_NUMBER_DESCRIPTOR, {sectionNumber: index + 1})}
								</h3>
								<div
									className={styles.editorSectionActions}
									data-flx="guild.self-assignable-roles.self-assignable-role-sections-editor.editor-section-actions"
								>
									<Button
										square={true}
										small={true}
										variant="secondary"
										icon={
											<ArrowUpIcon
												size={remFromPx(16)}
												weight="bold"
												data-flx="guild.self-assignable-roles.self-assignable-role-sections-editor.arrow-up-icon"
											/>
										}
										aria-label={i18n._(MOVE_SECTION_UP_DESCRIPTOR)}
										disabled={index === 0}
										onClick={() => moveSection(index, -1)}
										data-flx="guild.self-assignable-roles.self-assignable-role-sections-editor.button.move-section"
									/>
									<Button
										square={true}
										small={true}
										variant="secondary"
										icon={
											<ArrowDownIcon
												size={remFromPx(16)}
												weight="bold"
												data-flx="guild.self-assignable-roles.self-assignable-role-sections-editor.arrow-down-icon"
											/>
										}
										aria-label={i18n._(MOVE_SECTION_DOWN_DESCRIPTOR)}
										disabled={index === draft.length - 1}
										onClick={() => moveSection(index, 1)}
										data-flx="guild.self-assignable-roles.self-assignable-role-sections-editor.button.move-section--2"
									/>
									<Button
										square={true}
										small={true}
										variant="secondary"
										icon={
											<TrashIcon
												size={remFromPx(16)}
												weight="bold"
												data-flx="guild.self-assignable-roles.self-assignable-role-sections-editor.trash-icon"
											/>
										}
										aria-label={i18n._(DELETE_SECTION_DESCRIPTOR)}
										onClick={() => setDraft((current) => current.filter((candidate) => candidate.key !== section.key))}
										data-flx="guild.self-assignable-roles.self-assignable-role-sections-editor.button.set-draft"
									/>
								</div>
							</div>
							<Textarea
								label={i18n._(DESCRIPTION_DESCRIPTOR)}
								placeholder={i18n._(SECTION_TEXT_PLACEHOLDER_DESCRIPTOR)}
								value={section.content}
								maxLength={SELF_ASSIGNABLE_ROLE_SECTION_CONTENT_MAX_LENGTH}
								showCharacterCount={true}
								minRows={2}
								maxRows={8}
								onChange={(event) => {
									const content = event.target.value;
									updateSection(section.key, (current) => ({...current, content}));
								}}
								data-flx="guild.self-assignable-roles.self-assignable-role-sections-editor.textarea.update-section"
							/>
							<div
								className={styles.editorRoles}
								data-flx="guild.self-assignable-roles.self-assignable-role-sections-editor.editor-roles"
							>
								<p
									className={formStyles.label}
									data-flx="guild.self-assignable-roles.self-assignable-role-sections-editor.editor-roles-label"
								>
									{i18n._(ROLES_DESCRIPTOR)}
								</p>
								<EditorRoleList
									sectionKey={section.key}
									data-flx="guild.self-assignable-roles.self-assignable-role-sections-editor.editor-role-list"
								>
									{rows.map(({role, description}) => (
										<EditorRoleRow
											key={role.id}
											sectionKey={section.key}
											role={role}
											description={description}
											order={roleOrder}
											reorderable={rows.length > 1 && !saving}
											onDescriptionChange={(roleId, value) => setRoleDescription(section.key, roleId, value)}
											onRemove={(roleId) => setRoleInSection(section.key, roleId, false)}
											onDragStart={handleRoleDragStart}
											onMoveRole={handleMoveRole}
											onDragEnd={handleRoleDragEnd}
											data-flx="guild.self-assignable-roles.self-assignable-role-sections-editor.editor-role-row"
										/>
									))}
									<FocusRing
										offset={-2}
										data-flx="guild.self-assignable-roles.self-assignable-role-sections-editor.focus-ring--2"
									>
										<button
											type="button"
											className={styles.addRoleButton}
											disabled={rows.length >= MAX_SELF_ASSIGNABLE_ROLES_PER_SECTION}
											onClick={(event) => handleAddRole(event, section)}
											data-flx="guild.self-assignable-roles.self-assignable-role-sections-editor.add-role-button"
										>
											<PlusIcon
												size={remFromPx(14)}
												weight="bold"
												data-flx="guild.self-assignable-roles.self-assignable-role-sections-editor.plus-icon"
											/>
											<span data-flx="guild.self-assignable-roles.self-assignable-role-sections-editor.span">
												<Trans>Add role</Trans>
											</span>
										</button>
									</FocusRing>
								</EditorRoleList>
								{rows.length === 0 && (
									<p
										className={formStyles.errorText}
										data-flx="guild.self-assignable-roles.self-assignable-role-sections-editor.field-error"
									>
										{i18n._(ADD_AT_LEAST_ONE_ROLE_DESCRIPTOR)}
									</p>
								)}
							</div>
							<StepSliderRow
								label={i18n._(ROLE_LIMIT_LABEL_DESCRIPTOR)}
								stops={roleLimitStops(rows.length, section.maxRoles)}
								value={section.maxRoles ?? 0}
								defaultValue={0}
								labelledStops={[]}
								formatValue={(limit) => formatRoleLimit(i18n, limit)}
								formatAccessibleValue={(limit) => formatRoleLimit(i18n, limit)}
								onChange={(limit) =>
									updateSection(section.key, (current) => ({...current, maxRoles: limit === 0 ? null : limit}))
								}
								dataFlx="guild.self-assignable-roles.self-assignable-role-sections-editor.role-limit"
								data-flx="guild.self-assignable-roles.self-assignable-role-sections-editor.step-slider-row.update-section"
							/>
							<RequiredRolesField
								guild={guild}
								roleIds={section.requiredRoleIds}
								onAdd={(event) => handleAddRequiredRole(event, section)}
								onRemove={(roleId) => setRequiredRoleInSection(section.key, roleId, false)}
								data-flx="guild.self-assignable-roles.self-assignable-role-sections-editor.required-roles-field"
							/>
						</section>
					);
				})}
				<div
					className={styles.editorFooter}
					data-flx="guild.self-assignable-roles.self-assignable-role-sections-editor.editor-footer"
				>
					<Button
						variant="secondary"
						small={true}
						leftIcon={
							<PlusIcon
								size={remFromPx(16)}
								weight="bold"
								data-flx="guild.self-assignable-roles.self-assignable-role-sections-editor.plus-icon--2"
							/>
						}
						disabled={draft.length >= MAX_SELF_ASSIGNABLE_ROLE_SECTIONS}
						onClick={() => setDraft((current) => [...current, createSection()])}
						data-flx="guild.self-assignable-roles.self-assignable-role-sections-editor.button.set-draft--2"
					>
						<Trans>Add section</Trans>
					</Button>
					<div
						className={styles.editorFooterActions}
						data-flx="guild.self-assignable-roles.self-assignable-role-sections-editor.editor-footer-actions"
					>
						<Button
							variant="secondary"
							small={true}
							disabled={saving}
							onClick={onClose}
							data-flx="guild.self-assignable-roles.self-assignable-role-sections-editor.button.close"
						>
							{i18n._(CANCEL_DESCRIPTOR)}
						</Button>
						<Button
							small={true}
							submitting={saving}
							disabled={!isDirty || hasInvalidSection}
							onClick={handleSave}
							data-flx="guild.self-assignable-roles.self-assignable-role-sections-editor.button.save"
						>
							<Trans>Save changes</Trans>
						</Button>
					</div>
				</div>
				{saveError && (
					<p
						className={formStyles.errorText}
						role="alert"
						data-flx="guild.self-assignable-roles.self-assignable-role-sections-editor.field-error--2"
					>
						{saveError}
					</p>
				)}
				{isMobile && (
					<SelfAssignableRolePickerSheet
						isOpen={pickerSection != null}
						onClose={() => setPicker(null)}
						guildId={guild.id}
						unavailableRoleIds={
							pickerSection && !picker?.required ? getUnavailableRoleIds(pickerSection.key) : new Set()
						}
						allowUnmanageable={picker?.required === true}
						selectedRoleIds={picker?.required ? (pickerSection?.requiredRoleIds ?? []) : pickerRoleIds}
						onChange={(roleId, checked) => {
							if (!pickerSection) return;
							if (picker?.required) setRequiredRoleInSection(pickerSection.key, roleId, checked);
							else setRoleInSection(pickerSection.key, roleId, checked);
						}}
						data-flx="guild.self-assignable-roles.self-assignable-role-sections-editor.self-assignable-role-picker-sheet"
					/>
				)}
			</>
		);
	},
);
