import { observer } from "mobx-react-lite";
import * as styles from "./PersonaPickerPopout.module.css";
import type { Channel } from "@app/features/channel/models/Channel";
import Guilds from "@app/features/guild/state/Guilds";
import React, { useCallback, useEffect, useMemo, useRef, useState } from "react";
import Personas from "@app/features/user/state/Personas";
import { Persona } from "../../models/Persona";
import Users from "@app/features/user/state/Users";
import { Avatar } from "@app/features/ui/components/Avatar";
import { id } from "react-day-picker/locale";
import { CrownIcon, PencilIcon } from "@phosphor-icons/react";
import { isKeyboardActivationKey } from "@app/features/input/utils/KeyboardUtils";
import FocusRing from "@app/features/ui/focus_ring/FocusRing";
import GuildMembers from "@app/features/member/state/GuildMembers";
import { runInAction } from "mobx";
import { fetchUserPersonas } from "../../commands/Personas";
import { Tooltip } from "@app/features/ui/tooltip/Tooltip";
import { useLingui } from "@lingui/react";
import { msg } from "@lingui/core/macro";
import { Button } from "@app/features/ui/button/Button";
import { handleDeepLinkUrl } from "@app/features/navigation/utils/DeepLinkUtils";
import { Input } from "@app/features/ui/components/form/FormInput";
import { Trans } from "@lingui/react/macro";
import { SensitiveContentChoiceRow } from "@app/features/user/components/modals/tabs/privacy_safety_tab/SensitiveContentTab";
import { PersonaSettings_LatchMode } from "@fluxer/schema/src/gen/fluxer/user/preferences/v1/preferences_pb.js";
import { LATCH_MANUAL_DESCRIPTOR, LATCH_OFF_DESCRIPTOR, LATCH_TRIGGER_SWITCHING_DESCRIPTOR } from "@app/features/user/components/modals/tabs/PersonasTab";
import { clsx } from "clsx";
import PersonaPickerMobile from "../../state/PersonaPickerMobile";

const MAIN_ACCOUNT_DESCRIPTOR = msg({
	message: "This is your account.",
	comment: "Tooltip on the crown icon on the persona entry that represents your main account."
});
export const PERSONA_FILTER_PLACEHOLDER_DESCRIPTOR = msg({
	message: "Names, pronouns, or triggers",
	comment: "Placeholder text to hint at what can be used to filter personas in the list."
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

export const PersonaPickerPopout = observer<PersonaPickerPopoutProps>(({ channel, onSelect, ...props }) => {
	const i18n = useLingui();
	const guild = useMemo(() => channel.guildId ? Guilds.getGuild(channel.guildId) : null, [channel.guildId]);
	//const partner = useMemo(() => channel.isDM() ? channel.getRecipientId() || null : null, [channel.id]);
	const user = useMemo(() => Users.getCurrentUser(), []);
	const member = useMemo(() => guild && user && GuildMembers.getMember(guild.id, user.id), [guild, user]);
	if (!user) return;
	const rootPersona = useMemo(() => new Persona({
		id: "",
		internal_name: user.username,
		avatar: member?.avatar || user.avatar,
		display_name: member?.nick || user.displayName,
		tags: ["main", "primary", "account"],
		triggers: [{prefix: "\\", suffix: null}],
		pronouns: user.pronouns || null,
		bio: null, avatar_color: null, banner_color: null
	}), [user]);
	const [globalPersonas, setGlobalPersonas] = useState(() => Personas.getOwnPersonas());
	const [filter, setFilter] = useState("");
	const selectedPersonaId = props.selectedId ?? Personas.getGlobalActivePersona()?.id ?? "";
	const filteredPersonas = useMemo(() => {
		const sources = new Map([rootPersona, ...globalPersonas].map((v) => [v.id, v]));
		if (filter === "") return [...sources.values()];
		const sourceValues = [...sources.values()];
		const termedSources = new Map(sourceValues.map((v) => [
			v.id,
			[v.internal_name, v.display_name, v.pronouns, ...v.triggers.map((t) => `${t.prefix||""} ${t.suffix||""}`), ...v.tags].join(" ").toLowerCase()
		]));
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
		})
	}, [user]);

	const Item = useCallback(({ persona, selected, onSelect }: PersonaPickerItemProps) => {
		const onKeyDown = useCallback((e: React.KeyboardEvent) => {
			isKeyboardActivationKey(e.key) && onSelect();
		}, [onSelect, persona]);
		return <FocusRing>
			<div
				role="option"
				tabIndex={0}
				className={styles.item}
				onClick={onSelect}
				onKeyDown={onKeyDown}
				data-selected={selected || undefined}
			>
				<Avatar
					className={styles.avatar}
					user={user}
					guildId={channel.guildId}
					size={32}
					personaId={persona.id === "" ? undefined : persona.id}
					personaAvatar={persona.id === "" ? undefined : persona.avatar}
				/>
				<div className={styles.nameColumn}>
					<span className={styles.primaryName}>{persona.internal_name}</span>
					<span className={styles.secondaryName}>
						{persona.display_name !== persona.internal_name && persona.display_name}
						{persona.id === "" && <Tooltip
							text={i18n._(MAIN_ACCOUNT_DESCRIPTOR)}
						>
							<CrownIcon size={16} className={styles.accountIcon} color="var(--accent-primary, #BA5AF2)" />
						</Tooltip>}
					</span>
				</div>
			</div>
		</FocusRing>;
	}, [globalPersonas]);

	return <div className={clsx(styles.root, props.isMobile && styles.mobile)}>
		<div className={styles.header}>
			<SensitiveContentChoiceRow
				className={styles.latchModes}
				dataFlx="persona-picker.latch-mode-choice-row"
				label="Trigger Behavior"
				value={Personas.latchMode}
				onChange={(v) => Personas.latchMode = v}
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
			/>
			<Input
				label={<Trans>Filter personas</Trans>}
				placeholder={i18n._(PERSONA_FILTER_PLACEHOLDER_DESCRIPTOR)}
				value={filter}
				type="text"
				onChange={(e) => setFilter(e.target.value)}
			/>
		</div>
		<div className={styles.scrollingArea}>
			{filteredPersonas.map((v) => <Item
				key={v.id}
				persona={v}
				selected={selectedPersonaId === v.id}
				onSelect={() => onSelect(v.id)}
			/>)}
		</div>
		{props.isMobile ? <Button
				fitContainer
				variant="secondary"
				onClick={() => {
					handleDeepLinkUrl("fluxer://settings/user?tab=personas");
					PersonaPickerMobile.close();
				}}
				leftIcon={<PencilIcon />}
			>Manage Personas</Button>
		: <div className={styles.footer}>
			<Button
				compact
				variant="ghost"
				onClick={() => {handleDeepLinkUrl("fluxer://settings/user?tab=personas")}}
				leftIcon={<PencilIcon />}
			>Manage Personas</Button>
		</div>}
	</div>
});
