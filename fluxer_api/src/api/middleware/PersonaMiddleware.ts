import {createMiddleware} from 'hono/factory';
import {GuildRepository} from '../guild/repositories/GuildRepository';
import {PersonaRepository} from '../personas/repositories/PersonaRepository';
import {PersonaService} from '../personas/services/PersonaService';
import type {HonoEnv} from '../types/HonoEnv';
import {UserAccountRepository} from '../user/repositories/account/UserAccountRepository';
import {getKVClient} from './ServiceRegistry';

export const PersonaMiddleware = createMiddleware<HonoEnv>(async (ctx, next) => {
	ctx.set(
		'personaService',
		new PersonaService(
			new PersonaRepository(),
			ctx.get('snowflakeService'),
			ctx.get('entityAssetService'),
			ctx.get('userCacheService'),
			ctx.get('requestCache'),
			ctx.get('storageService'),
			ctx.get('rateLimitService'),
			ctx.get('limitConfigService'),
			new UserAccountRepository(getKVClient()),
			new GuildRepository(ctx.get('requestCache')),
		),
	);
	return next();
});
