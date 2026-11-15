// SPDX-License-Identifier: AGPL-3.0-or-later

import '@app/app/BootShell.css';
import '@fluxer/fonts/css/fluxer-sans.css';
import '@fluxer/fonts/css/fluxer-mono.css';
import '@fluxer/fonts/css/variables.css';
import '@fluxer/fonts/css/locale-fallbacks.css';
import '@app/app/font-fallback.css';
import '@app/app/fonts/fallback/fallback-faces.css';
import '@app/app/globals.css';
import '@app/features/theme/styles/generated/color-system.css';
import '@app/features/theme/styles/generated/message-layout.css';
import '@app/features/theme/styles/preflight.css';

void import(/* webpackMode: "eager" */ '@app/indexMain');
