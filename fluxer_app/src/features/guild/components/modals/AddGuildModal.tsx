// SPDX-License-Identifier: AGPL-3.0-or-later

import * as Modal from '@app/features/app/components/dialogs/Modal';
import RuntimeConfig from '@app/features/app/state/RuntimeConfig';
import styles from '@app/features/guild/components/modals/AddGuildModal.module.css';
import {GuildCreateForm} from '@app/features/guild/components/modals/add_guild_modal/GuildCreateForm';
import {GuildJoinForm} from '@app/features/guild/components/modals/add_guild_modal/GuildJoinForm';
import {LandingView} from '@app/features/guild/components/modals/add_guild_modal/LandingView';
import {
	ADD_A_COMMUNITY_DESCRIPTOR,
	ADD_GUILD_VIEW_ORDER,
	type AddGuildModalView,
	CREATE_A_COMMUNITY_DESCRIPTOR,
	IMPORT_THE_OTHER_PLATFORM_TEMPLATE_DESCRIPTOR,
	JOIN_A_COMMUNITY_DESCRIPTOR,
	ModalFooterContext,
} from '@app/features/guild/components/modals/add_guild_modal/shared';
import {TemplateImportForm} from '@app/features/guild/components/modals/add_guild_modal/TemplateImportForm';
import {SteppedCarousel} from '@app/features/ui/stepped_carousel/SteppedCarousel';
import {THE_OTHER_PLATFORM} from '@fluxer/constants/src/ExternalPlatformConstants';
import {useLingui} from '@lingui/react/macro';
import {AnimatePresence, motion, type Transition, useReducedMotion} from 'framer-motion';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useCallback, useMemo, useState} from 'react';

export type {AddGuildModalView} from '@app/features/guild/components/modals/add_guild_modal/shared';

const footerRevealTransition: Transition = {
	type: 'spring',
	stiffness: 460,
	damping: 40,
	mass: 0.7,
};
const instantTransition: Transition = {
	duration: 0,
};

export const AddGuildModal = observer(({initialView = 'landing'}: {initialView?: AddGuildModalView} = {}) => {
	const {i18n} = useLingui();
	const shouldReduceMotion = useReducedMotion();
	const [view, setView] = useState<AddGuildModalView>(initialView);
	const [footerContent, setFooterContent] = useState<React.ReactNode>(null);
	const getTitle = (): string => {
		switch (view) {
			case 'landing':
				return i18n._(ADD_A_COMMUNITY_DESCRIPTOR);
			case 'create_guild':
				return i18n._(CREATE_A_COMMUNITY_DESCRIPTOR);
			case 'join_guild':
				return i18n._(JOIN_A_COMMUNITY_DESCRIPTOR);
			case 'import_template':
				return i18n._(IMPORT_THE_OTHER_PLATFORM_TEMPLATE_DESCRIPTOR, {theOtherPlatform: THE_OTHER_PLATFORM});
			default:
				return i18n._(ADD_A_COMMUNITY_DESCRIPTOR);
		}
	};
	const handleBack = useCallback(() => setView('landing'), []);
	const contextValue = useMemo(() => ({setFooterContent, onBack: handleBack}), [handleBack]);
	if (RuntimeConfig.singleCommunityEnabled) {
		return null;
	}
	const renderView = (): React.ReactNode => {
		switch (view) {
			case 'landing':
				return <LandingView onViewChange={setView} data-flx="guild.add-guild-modal.render-view.landing-view" />;
			case 'create_guild':
				return <GuildCreateForm data-flx="guild.add-guild-modal.render-view.guild-create-form" />;
			case 'join_guild':
				return <GuildJoinForm data-flx="guild.add-guild-modal.render-view.guild-join-form" />;
			case 'import_template':
				return <TemplateImportForm data-flx="guild.add-guild-modal.render-view.template-import-form" />;
		}
	};
	return (
		<ModalFooterContext.Provider value={contextValue}>
			<Modal.Root size="small" centered data-flx="guild.add-guild-modal.modal-root">
				<Modal.Header title={getTitle()} data-flx="guild.add-guild-modal.modal-header" />
				<Modal.Content contentClassName={styles.content} data-flx="guild.add-guild-modal.modal-content">
					<SteppedCarousel
						step={view}
						steps={ADD_GUILD_VIEW_ORDER}
						focusOnStepChange
						data-flx="guild.add-guild-modal.stepped-carousel"
					>
						{renderView()}
					</SteppedCarousel>
				</Modal.Content>
				<AnimatePresence initial={false} data-flx="guild.add-guild-modal.animate-presence">
					{footerContent && (
						<motion.div
							key="footer"
							className={styles.footerReveal}
							initial={{height: 0, opacity: 0}}
							animate={{height: 'auto', opacity: 1}}
							exit={{height: 0, opacity: 0}}
							transition={shouldReduceMotion ? instantTransition : footerRevealTransition}
							data-flx="guild.add-guild-modal.footer-reveal"
						>
							<Modal.Footer data-flx="guild.add-guild-modal.modal-footer">{footerContent}</Modal.Footer>
						</motion.div>
					)}
				</AnimatePresence>
			</Modal.Root>
		</ModalFooterContext.Provider>
	);
});
