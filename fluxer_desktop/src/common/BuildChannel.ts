// SPDX-License-Identifier: AGPL-3.0-or-later

export type BuildChannel = 'stable' | 'canary' | 'development';

declare const __FLUXER_BUILD_CHANNEL__: BuildChannel;

export const BUILD_CHANNEL = __FLUXER_BUILD_CHANNEL__;
