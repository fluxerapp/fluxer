// SPDX-License-Identifier: AGPL-3.0-or-later

import {EDIT_SECTIONS_DESCRIPTOR} from '@app/features/guild/components/self_assignable_roles/SelfAssignableRoleMessages';
import {SelfAssignableRoleSectionsEditor} from '@app/features/guild/components/self_assignable_roles/SelfAssignableRoleSectionsEditor';
import styles from '@app/features/guild/components/self_assignable_roles/SelfAssignableRoles.module.css';
import {SelfAssignableRolesProfileCard} from '@app/features/guild/components/self_assignable_roles/SelfAssignableRolesProfileCard';
import {
	RoleChipLabel,
	SelfAssignableRolesEmptyState,
} from '@app/features/guild/components/self_assignable_roles/SelfAssignableRolesShared';
import {SelfAssignableRolesVerificationNotice} from '@app/features/guild/components/self_assignable_roles/SelfAssignableRolesVerificationNotice';
import type {GuildSelfAssignableRoleSection} from '@app/features/guild/models/Guild';
import type {GuildRole} from '@app/features/guild/models/GuildRole';
import Guilds from '@app/features/guild/state/Guilds';
import GuildVerification from '@app/features/guild/state/GuildVerification';
import {
	canEditSelfAssignableRoles,
	getAvailableSelfAssignableRoleSections,
	getVisibleSelfAssignableRoleSections,
} from '@app/features/guild/utils/SelfAssignableRoles';
import * as GuildMemberCommands from '@app/features/member/commands/GuildMemberCommands';
import GuildMembers from '@app/features/member/state/GuildMembers';
import {SafeMarkdown} from '@app/features/messaging/components/markdown';
import {MarkdownContext} from '@app/features/messaging/components/markdown/renderers/RendererTypes';
import {remFromPx} from '@app/features/theme/layout/RemFromPx';
import markupStyles from '@app/features/theme/styles/Markup.module.css';
import {Button} from '@app/features/ui/button/Button';
import * as ToastCommands from '@app/features/ui/commands/ToastCommands';
import FocusRing from '@app/features/ui/focus_ring/FocusRing';
import {Tooltip} from '@app/features/ui/tooltip/Tooltip';
import Users from '@app/features/user/state/Users';
import {extractErrorMessage} from '@app/lib/forms';
import {ME} from '@fluxer/constants/src/AppConstants';
import {evaluateSelfAssignableRolePick} from '@fluxer/constants/src/SelfAssignableRoleRules';
import {msg} from '@lingui/core/macro';
import {Trans, useLingui} from '@lingui/react/macro';
import {CheckIcon, PencilSimpleIcon, PlusIcon} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useEffect, useId, useState} from 'react';

const PICK_ROLES_INTRO_DESCRIPTOR = msg({
	message: 'Pick roles for yourself. You can change them at any time.',
	comment: 'Intro text at the top of the Roles page, where members give themselves self-assignable roles.',
});
const EMPTY_SECTION_HINT_DESCRIPTOR = msg({
	message: "Members can't see this section until it has a role.",
	comment: 'Shown on the Roles page under a self-assignable role section whose roles were all deleted.',
});
const CHOOSE_ONE_DESCRIPTOR = msg({
	message: 'Choose one',
	comment:
		'Hint on the Roles page above the roles of a self-assignable role section where members can only have one role. Choosing another role swaps it.',
});
const CHOOSE_UP_TO_DESCRIPTOR = msg({
	message: 'Choose up to {count}',
	comment:
		'Hint on the Roles page above the roles of a self-assignable role section that limits how many of its roles a member can have. {count} is the limit and is at least 2.',
});
const LOCKED_SECTION_HINT_DESCRIPTOR = msg({
	message: 'Only members with a required role can see this section.',
	comment: 'Shown on the Roles page under a self-assignable role section that only members with certain roles can see.',
});
const NO_SECTIONS_FOR_EDITORS_DESCRIPTOR = msg({
	message: 'Add a section with some text and roles, and members can give themselves those roles here.',
	comment: 'Empty state of the Roles page for community managers when no self-assignable role sections exist.',
});
const NO_ROLES_TO_PICK_DESCRIPTOR = msg({
	message: 'There are no roles to pick right now.',
	comment: 'Empty state of the Roles page for members when the community has no self-assignable roles.',
});

interface RoleToggleProps {
	readonly role: GuildRole;
	readonly description: string;
	readonly layout: 'chip' | 'card';
	readonly selected: boolean;
	readonly pending: boolean;
	readonly disabled: boolean;
	readonly onToggle: () => void;
}

const RoleToggle: React.FC<RoleToggleProps> = observer(function RoleToggle({
	role,
	description,
	layout,
	selected,
	pending,
	disabled,
	onToggle,
}) {
	const nameId = useId();
	const descriptionId = useId();
	const card = layout === 'card';
	const hasDescription = card && description.length > 0;
	const label = (
		<>
			<RoleChipLabel
				role={role}
				name={role.name}
				nameId={nameId}
				data-flx="guild.self-assignable-roles.self-assignable-roles-view.role-toggle.role-chip-label"
			/>
			{selected && (
				<CheckIcon
					className={styles.roleChipCheck}
					size={remFromPx(14)}
					weight="bold"
					data-flx="guild.self-assignable-roles.self-assignable-roles-view.role-toggle.role-chip-check"
				/>
			)}
		</>
	);
	return (
		<FocusRing offset={-2} data-flx="guild.self-assignable-roles.self-assignable-roles-view.role-toggle.focus-ring">
			<button
				type="button"
				className={clsx(card ? styles.roleCard : styles.roleChip, selected && styles.roleToggleSelected)}
				aria-pressed={selected}
				aria-labelledby={hasDescription ? nameId : undefined}
				aria-describedby={hasDescription ? descriptionId : undefined}
				disabled={pending || disabled}
				onClick={onToggle}
				data-flx="guild.self-assignable-roles.self-assignable-roles-view.role-toggle.role-card.toggle.button"
			>
				{card ? (
					<span
						className={styles.roleCardHeader}
						data-flx="guild.self-assignable-roles.self-assignable-roles-view.role-toggle.role-card-header"
					>
						{label}
					</span>
				) : (
					label
				)}
				{hasDescription && (
					<span
						id={descriptionId}
						className={styles.roleCardDescription}
						dir="auto"
						data-flx="guild.self-assignable-roles.self-assignable-roles-view.role-toggle.role-card-description"
					>
						{description}
					</span>
				)}
			</button>
		</FocusRing>
	);
});

interface SelfAssignableRolesViewProps {
	readonly guildId: string;
	readonly variant?: 'page' | 'modal';
}

export const SelfAssignableRolesView: React.FC<SelfAssignableRolesViewProps> = observer(
	function SelfAssignableRolesView({guildId, variant = 'page'}) {
		const {i18n} = useLingui();
		const guild = Guilds.getGuild(guildId);
		const currentUserId = Users.currentUser?.id ?? null;
		const member = currentUserId ? GuildMembers.getMember(guildId, currentUserId) : null;
		const canEdit = canEditSelfAssignableRoles(guildId);
		const verification = GuildVerification.getVerificationLevelStatus(guildId);
		const [editing, setEditing] = useState<{startWithNewSection: boolean} | null>(null);
		const [optimisticRoles, setOptimisticRoles] = useState<ReadonlyMap<string, boolean>>(new Map());
		const [pendingRoleIds, setPendingRoleIds] = useState<ReadonlySet<string>>(new Set());
		const memberRoles = member?.roles;
		useEffect(() => {
			if (!memberRoles) return;
			setOptimisticRoles((current) => {
				let changed = false;
				const next = new Map(current);
				for (const [roleId, wanted] of current) {
					if (memberRoles.has(roleId) === wanted) {
						next.delete(roleId);
						changed = true;
					}
				}
				return changed ? next : current;
			});
		}, [memberRoles]);
		useEffect(() => {
			if (editing && !canEdit) setEditing(null);
		}, [editing, canEdit]);
		if (!guild) return null;
		const hasRole = (roleId: string): boolean => optimisticRoles.get(roleId) ?? memberRoles?.has(roleId) ?? false;
		const toggleRole = async (roleId: string, section: GuildSelfAssignableRoleSection) => {
			const wanted = !hasRole(roleId);
			const pick = evaluateSelfAssignableRolePick({
				sectionRoleIds: section.roles.map((role) => role.id),
				maxRoles: section.maxRoles,
				roleId,
				hasRole,
			});
			const swapped = wanted && pick.allowed ? pick.replacedRoleIds : [];
			setPendingRoleIds((current) => new Set(current).add(roleId));
			setOptimisticRoles((current) => {
				const next = new Map(current).set(roleId, wanted);
				for (const id of swapped) next.set(id, false);
				return next;
			});
			try {
				if (wanted) {
					await GuildMemberCommands.addRole(guildId, ME, roleId);
				} else {
					await GuildMemberCommands.removeRole(guildId, ME, roleId);
				}
			} catch (error) {
				setOptimisticRoles((current) => {
					const next = new Map(current);
					for (const id of [roleId, ...swapped]) next.delete(id);
					return next;
				});
				ToastCommands.error(extractErrorMessage(i18n, error));
			} finally {
				setPendingRoleIds((current) => {
					const next = new Set(current);
					next.delete(roleId);
					return next;
				});
			}
		};
		const sections = canEdit
			? getAvailableSelfAssignableRoleSections(guild)
			: getVisibleSelfAssignableRoleSections(guild, memberRoles ?? new Set<string>());
		let body: React.ReactNode;
		if (editing) {
			body = (
				<SelfAssignableRoleSectionsEditor
					guild={guild}
					startWithNewSection={editing.startWithNewSection}
					onClose={() => setEditing(null)}
					data-flx="guild.self-assignable-roles.self-assignable-roles-view.self-assignable-role-sections-editor"
				/>
			);
		} else if (sections.length === 0) {
			body = (
				<SelfAssignableRolesEmptyState
					message={i18n._(canEdit ? NO_SECTIONS_FOR_EDITORS_DESCRIPTOR : NO_ROLES_TO_PICK_DESCRIPTOR)}
					data-flx="guild.self-assignable-roles.self-assignable-roles-view.self-assignable-roles-empty-state"
				>
					{canEdit && (
						<Button
							small={true}
							leftIcon={
								<PlusIcon
									size={remFromPx(16)}
									weight="bold"
									data-flx="guild.self-assignable-roles.self-assignable-roles-view.plus-icon"
								/>
							}
							onClick={() => setEditing({startWithNewSection: true})}
							data-flx="guild.self-assignable-roles.self-assignable-roles-view.button.set-editing--2"
						>
							<Trans>Add section</Trans>
						</Button>
					)}
				</SelfAssignableRolesEmptyState>
			);
		} else {
			body = sections.map(({section, roles}) => {
				const limit = section.maxRoles != null && section.maxRoles < roles.length ? section.maxRoles : null;
				const sectionRoleIds = section.roles.map((role) => role.id);
				const canPick = (roleId: string) =>
					evaluateSelfAssignableRolePick({sectionRoleIds, maxRoles: section.maxRoles, roleId, hasRole}).allowed;
				const layout = roles.some(({description}) => description.length > 0) ? 'card' : 'chip';
				return (
					<section
						key={section.id}
						className={styles.section}
						data-flx="guild.self-assignable-roles.self-assignable-roles-view.section"
					>
						{section.content.length > 0 && (
							<div
								className={clsx(markupStyles.markup, styles.sectionContent)}
								data-flx="guild.self-assignable-roles.self-assignable-roles-view.section-content"
							>
								<SafeMarkdown
									content={section.content}
									options={{context: MarkdownContext.STANDARD_WITHOUT_JUMBO, guildId}}
									data-flx="guild.self-assignable-roles.self-assignable-roles-view.safe-markdown"
								/>
							</div>
						)}
						{canEdit && section.requiredRoleIds.length > 0 && (
							<p
								className={styles.sectionHint}
								data-flx="guild.self-assignable-roles.self-assignable-roles-view.section-hint"
							>
								{i18n._(LOCKED_SECTION_HINT_DESCRIPTOR)}
							</p>
						)}
						{limit != null && (
							<p
								className={styles.sectionHint}
								data-flx="guild.self-assignable-roles.self-assignable-roles-view.section-hint"
							>
								{limit === 1 ? i18n._(CHOOSE_ONE_DESCRIPTOR) : i18n._(CHOOSE_UP_TO_DESCRIPTOR, {count: limit})}
							</p>
						)}
						{roles.length > 0 ? (
							<div
								className={layout === 'card' ? styles.roleCards : styles.roleChips}
								data-flx="guild.self-assignable-roles.self-assignable-roles-view.role-cards"
							>
								{roles.map(({role, description}) => (
									<RoleToggle
										key={role.id}
										role={role}
										description={description}
										layout={layout}
										selected={hasRole(role.id)}
										pending={pendingRoleIds.has(role.id)}
										disabled={!verification.canAccess || !canPick(role.id)}
										onToggle={() => void toggleRole(role.id, section)}
										data-flx="guild.self-assignable-roles.self-assignable-roles-view.role-toggle"
									/>
								))}
							</div>
						) : (
							<p
								className={styles.sectionHint}
								data-flx="guild.self-assignable-roles.self-assignable-roles-view.section-hint"
							>
								{i18n._(EMPTY_SECTION_HINT_DESCRIPTOR)}
							</p>
						)}
					</section>
				);
			});
		}
		return (
			<div
				className={styles.layoutContainer}
				data-flx="guild.self-assignable-roles.self-assignable-roles-view.layout-container"
			>
				<div
					className={clsx(styles.layout, variant === 'modal' && styles.layoutInModal)}
					data-flx="guild.self-assignable-roles.self-assignable-roles-view.layout"
				>
					<div
						className={styles.profileColumn}
						data-flx="guild.self-assignable-roles.self-assignable-roles-view.profile-column"
					>
						<SelfAssignableRolesProfileCard
							guildId={guildId}
							data-flx="guild.self-assignable-roles.self-assignable-roles-view.self-assignable-roles-profile-card"
						/>
					</div>
					<div className={styles.view} data-flx="guild.self-assignable-roles.self-assignable-roles-view.view">
						<div className={styles.intro} data-flx="guild.self-assignable-roles.self-assignable-roles-view.intro">
							<p
								className={styles.introText}
								data-flx="guild.self-assignable-roles.self-assignable-roles-view.intro-text"
							>
								{i18n._(PICK_ROLES_INTRO_DESCRIPTOR)}
							</p>
							{canEdit && !editing && (
								<Tooltip
									text={i18n._(EDIT_SECTIONS_DESCRIPTOR)}
									data-flx="guild.self-assignable-roles.self-assignable-roles-view.tooltip"
								>
									<Button
										square={true}
										small={true}
										variant="secondary"
										icon={
											<PencilSimpleIcon
												size={remFromPx(16)}
												weight="bold"
												data-flx="guild.self-assignable-roles.self-assignable-roles-view.pencil-simple-icon"
											/>
										}
										aria-label={i18n._(EDIT_SECTIONS_DESCRIPTOR)}
										onClick={() => setEditing({startWithNewSection: false})}
										data-flx="guild.self-assignable-roles.self-assignable-roles-view.button.set-editing"
									/>
								</Tooltip>
							)}
						</div>
						{!editing && sections.length > 0 && (
							<SelfAssignableRolesVerificationNotice
								status={verification}
								data-flx="guild.self-assignable-roles.self-assignable-roles-view.self-assignable-roles-verification-notice"
							/>
						)}
						{body}
					</div>
				</div>
			</div>
		);
	},
);
