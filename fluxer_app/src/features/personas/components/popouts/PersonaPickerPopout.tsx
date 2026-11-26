import Accessibility from '@app/features/accessibility/state/Accessibility';
import type {Channel} from '@app/features/channel/models/Channel';
import Guilds from '@app/features/guild/state/Guilds';
import {isKeyboardActivationKey} from '@app/features/input/utils/KeyboardUtils';
import GuildMembers from '@app/features/member/state/GuildMembers';
import {handleDeepLinkUrl} from '@app/features/navigation/utils/DeepLinkUtils';
import {CheckboxItem, SubMenu as MenuItemSubmenu} from '@app/features/ui/action_menu/ContextMenu';
import {MoreIcon} from '@app/features/ui/action_menu/ContextMenuIcons';
import {MenuGroup} from '@app/features/ui/action_menu/MenuGroup';
import {MenuGroups} from '@app/features/ui/action_menu/MenuGroups';
import {MenuItemRadio} from '@app/features/ui/action_menu/MenuItemRadio';
import {Button} from '@app/features/ui/button/Button';
import * as ContextMenuCommands from '@app/features/ui/commands/ContextMenuCommands';
import {Avatar} from '@app/features/ui/components/Avatar';
import {Input} from '@app/features/ui/components/form/FormInput';
import FocusRing from '@app/features/ui/focus_ring/FocusRing';
import {Tooltip} from '@app/features/ui/tooltip/Tooltip';
import {
	LATCH_MANUAL_DESCRIPTOR,
	LATCH_OFF_DESCRIPTOR,
	LATCH_TRIGGER_SWITCHING_DESCRIPTOR,
} from '@app/features/user/components/modals/tabs/advanced_settings_tab/AdvancedChatControls';
import {SensitiveContentChoiceRow} from '@app/features/user/components/modals/tabs/privacy_safety_tab/SensitiveContentTab';
import Personas from '@app/features/user/state/Personas';
import Users from '@app/features/user/state/Users';
import {MORE_ACTIONS_DESCRIPTOR} from '@app/features/user/utils/UserMessageDescriptors';
import {PersonaSettings_LatchMode} from '@fluxer/schema/src/gen/fluxer/user/preferences/v1/preferences_pb.js';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react';
import {Trans} from '@lingui/react/macro';
import {CrownIcon, PencilIcon} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import {runInAction} from 'mobx';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useCallback, useEffect, useMemo, useState} from 'react';
import {fetchUserPersonas} from '../../commands/Personas';
import {Persona} from '../../models/Persona';
import PersonaPickerMobile from '../../state/PersonaPickerMobile';
import * as styles from './PersonaPickerPopout.module.css';

const MAIN_ACCOUNT_DESCRIPTOR = msg({
	message: 'This is your account.',
	comment: 'Tooltip on the crown icon on the persona entry that represents your main account.',
});
export const PERSONA_FILTER_PLACEHOLDER_DESCRIPTOR = msg({
	message: 'Names, pronouns, or triggers',
	comment: 'Placeholder text to hint at what can be used to filter personas in the list.',
});
const TRIGGER_BEHAVIOR_DESCRIPTOR = msg({
	message: 'Trigger behavior',
	comment: 'Menu item for persona trigger behaviors in the persona picker.',
});

export interface PersonaPickerPopoutProps {
	channel: Channel;
	selectedId?: string;
	onSelect: (persona_id: string | null) => void;
	isMobile?: boolean;
}

interface PersonaPickerItemProps {
	persona: Persona;
	selected?: boolean;
	onSelect: () => void;
}

export const PersonaPickerPopout = observer<PersonaPickerPopoutProps>(({channel, onSelect, ...props}) => {
	const i18n = useLingui();
	const guild = useMemo(() => (channel.guildId ? Guilds.getGuild(channel.guildId) : null), [channel.guildId]);
	//const partner = useMemo(() => channel.isDM() ? channel.getRecipientId() || null : null, [channel.id]);
	const user = useMemo(() => Users.getCurrentUser(), []);
	const member = useMemo(() => guild && user && GuildMembers.getMember(guild.id, user.id), [guild, user]);
	if (!user) return;
	const rootPersona = useMemo(
		() =>
			new Persona({
				id: '',
				internal_name: user.username,
				avatar: member?.avatar || user.avatar,
				display_name: member?.nick || user.displayName,
				tags: ['main', 'primary', 'account'],
				triggers: [{prefix: '\\', suffix: null}],
				pronouns: user.pronouns || null,
				bio: null,
				avatar_color: null,
				banner_color: null,
			}),
		[user],
	);
	const [globalPersonas, setGlobalPersonas] = useState(() => Personas.getOwnPersonas());
	const [filter, setFilter] = useState('');
	const selectedPersonaId =
		props.selectedId ?? Personas.getActivePersonaFromContext(channel.id, channel.guildId)?.id ?? '';
	const filteredPersonas = useMemo(() => {
		const sources = new Map([rootPersona, ...globalPersonas].map((v) => [v.id, v]));
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
	}, [filter, globalPersonas]);

	useEffect(() => {
		fetchUserPersonas().then((personas) => {
			runInAction(() => {
				Personas.cachePersonas(personas);
				setGlobalPersonas(Personas.getOwnPersonas());
			});
		});
	}, [user]);

	const Item = useCallback(
		({persona, selected, onSelect}: PersonaPickerItemProps) => {
			const onKeyDown = useCallback(
				(e: React.KeyboardEvent) => {
					isKeyboardActivationKey(e.key) && onSelect();
				},
				[onSelect, persona],
			);
			return (
				<FocusRing data-flx="personas.persona-picker-popout.item.focus-ring">
					<div
						role="option"
						tabIndex={0}
						className={styles.item}
						onClick={onSelect}
						onKeyDown={onKeyDown}
						data-selected={selected || undefined}
						data-flx="personas.persona-picker-popout.item.item.select"
					>
						<Avatar
							className={styles.avatar}
							user={user}
							guildId={channel.guildId}
							size={32}
							personaId={persona.id === '' ? undefined : persona.id}
							personaAvatar={persona.id === '' ? undefined : persona.avatar}
							data-flx="personas.persona-picker-popout.item.avatar"
						/>
						<div className={styles.nameColumn} data-flx="personas.persona-picker-popout.item.name-column">
							<span /*className={styles.primaryName}*/ data-flx="personas.persona-picker-popout.item.span">
								{persona.internal_name}
							</span>
							<span className={styles.secondaryName} data-flx="personas.persona-picker-popout.item.secondary-name">
								<span data-flx="personas.persona-picker-popout.item.span--2">
									{persona.display_name !== persona.internal_name && persona.display_name}
								</span>
								{persona.id === '' && (
									<Tooltip
										text={i18n._(MAIN_ACCOUNT_DESCRIPTOR)}
										data-flx="personas.persona-picker-popout.item.tooltip"
									>
										<CrownIcon
											size={16}
											className={styles.accountIcon}
											color="var(--accent-primary, #BA5AF2)"
											data-flx="personas.persona-picker-popout.item.account-icon"
										/>
									</Tooltip>
								)}
							</span>
						</div>
					</div>
				</FocusRing>
			);
		},
		[globalPersonas],
	);

	const openMoreMenu = useCallback(
		(event: React.MouseEvent<HTMLElement>) => {
			ContextMenuCommands.openFromEvent(
				event,
				(_) => (
					<MenuGroups>
						<MenuGroup>
							{channel.guildId && (
								<CheckboxItem
									checked={Personas.hasSpecificActivePersona(channel.guildId)}
									onCheckedChange={(shouldSet) =>
										shouldSet
											? Personas.setSpecificActivePersona(channel.guildId!, selectedPersonaId)
											: Personas.clearSpecificActivePersona(channel.guildId!)
									}
									data-flx="user.component-gallery-tab.open-context-menu.checkbox-item"
								>
									<Trans>Set for only this community</Trans>
								</CheckboxItem>
							)}
							<CheckboxItem
								checked={Personas.hasSpecificActivePersona(channel.id)}
								onCheckedChange={(shouldSet) =>
									shouldSet
										? Personas.setSpecificActivePersona(channel.id, selectedPersonaId)
										: Personas.clearSpecificActivePersona(channel.id)
								}
								data-flx="user.component-gallery-tab.open-context-menu.checkbox-item"
							>
								<Trans>Set for only this channel</Trans>
							</CheckboxItem>
						</MenuGroup>
						<MenuGroup data-flx="user.component-gallery-tab.open-context-menu.menu-group--3">
							<MenuItemSubmenu
								label={i18n._(TRIGGER_BEHAVIOR_DESCRIPTOR)}
								render={() => (
									<>
										<MenuItemRadio
											selected={Personas.latchMode === PersonaSettings_LatchMode.OFF}
											onSelect={() => (Personas.latchMode = PersonaSettings_LatchMode.OFF)}
											data-flx="user.component-gallery-tab.open-context-menu.menu-item-radio.set-radio-value"
										>
											{i18n._(LATCH_OFF_DESCRIPTOR)}
										</MenuItemRadio>
										<MenuItemRadio
											selected={Personas.latchMode === PersonaSettings_LatchMode.MANUAL}
											onSelect={() => (Personas.latchMode = PersonaSettings_LatchMode.MANUAL)}
											data-flx="user.component-gallery-tab.open-context-menu.menu-item-radio.set-radio-value"
										>
											{i18n._(LATCH_MANUAL_DESCRIPTOR)}
										</MenuItemRadio>
										<MenuItemRadio
											selected={Personas.latchMode === PersonaSettings_LatchMode.TRIGGER_SWITCHING}
											onSelect={() => (Personas.latchMode = PersonaSettings_LatchMode.TRIGGER_SWITCHING)}
											data-flx="user.component-gallery-tab.open-context-menu.menu-item-radio.set-radio-value"
										>
											{i18n._(LATCH_TRIGGER_SWITCHING_DESCRIPTOR)}
										</MenuItemRadio>
									</>
								)}
							/>
						</MenuGroup>
					</MenuGroups>
				),
				{align: 'bottom-right'},
			);
		},
		[Personas.latchMode, Personas.contextActivePersonaIds, selectedPersonaId, i18n],
	);

	return (
		<div className={clsx(styles.root, props.isMobile && styles.mobile)} data-flx="personas.persona-picker-popout.root">
			<div className={styles.header} data-flx="personas.persona-picker-popout.header">
				{Accessibility.showLatchInPersonaPicker && (
					<SensitiveContentChoiceRow
						className={styles.latchModes}
						dataFlx="persona-picker.latch-mode-choice-row"
						label="Trigger Behavior"
						description=""
						value={Personas.latchMode}
						onChange={(v) => (Personas.latchMode = v)}
						options={[
							{
								value: PersonaSettings_LatchMode.OFF,
								label: i18n._(LATCH_OFF_DESCRIPTOR),
							},
							{
								value: PersonaSettings_LatchMode.MANUAL,
								label: i18n._(LATCH_MANUAL_DESCRIPTOR),
							},
							{
								value: PersonaSettings_LatchMode.TRIGGER_SWITCHING,
								label: i18n._(LATCH_TRIGGER_SWITCHING_DESCRIPTOR),
							},
						]}
						data-flx="personas.persona-picker-popout.latch-modes"
					/>
				)}
				<Input
					label={<Trans>Filter personas</Trans>}
					placeholder={i18n._(PERSONA_FILTER_PLACEHOLDER_DESCRIPTOR)}
					value={filter}
					type="text"
					onChange={(e) => setFilter(e.target.value)}
					data-flx="personas.persona-picker-popout.input.set-filter.text"
				/>
			</div>
			<div className={styles.scrollingArea} data-flx="personas.persona-picker-popout.scrolling-area">
				{filteredPersonas.map((v) => (
					<Item
						key={v.id}
						persona={v}
						selected={selectedPersonaId === v.id}
						onSelect={() => onSelect(v.id)}
						data-flx="personas.persona-picker-popout.item.select"
					/>
				))}
			</div>
			{props.isMobile ? (
				<Button
					fitContainer
					variant="secondary"
					onClick={() => {
						handleDeepLinkUrl('fluxer://settings/user?tab=personas');
						PersonaPickerMobile.close();
					}}
					leftIcon={<PencilIcon data-flx="personas.persona-picker-popout.pencil-icon" />}
					data-flx="personas.persona-picker-popout.button.deep-link-url"
				>
					<Trans>Manage Personas</Trans>
				</Button>
			) : (
				<div className={styles.footer} data-flx="personas.persona-picker-popout.footer">
					<Button
						compact
						variant="ghost"
						onClick={() => {
							handleDeepLinkUrl('fluxer://settings/user?tab=personas');
						}}
						leftIcon={<PencilIcon data-flx="personas.persona-picker-popout.pencil-icon--2" />}
						data-flx="personas.persona-picker-popout.button.deep-link-url--2"
					>
						<Trans>Manage Personas</Trans>
					</Button>
					<Button
						compact
						square
						variant="ghost"
						onClick={openMoreMenu}
						icon={<MoreIcon size={16} />}
						aria-label={i18n._(MORE_ACTIONS_DESCRIPTOR)}
					></Button>
				</div>
			)}
		</div>
	);
});
