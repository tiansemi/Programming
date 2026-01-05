/**
 * Système de logging centralisé avec couleurs pour le débogage
 */

const LOG_LEVELS = {
  DEBUG: { color: '#7C7C7C', name: 'DEBUG' },
  INFO: { color: '#0066CC', name: 'INFO' },
  SUCCESS: { color: '#00AA00', name: '✅ SUCCESS' },
  WARN: { color: '#FF8800', name: '⚠️ WARN' },
  ERROR: { color: '#FF0000', name: '❌ ERROR' },
}

const createLogger = (module) => ({
  debug: (message, data = null) => {
    const style = `color: ${LOG_LEVELS.DEBUG.color}; font-weight: bold;`
    console.log(`%c[${module}] ${LOG_LEVELS.DEBUG.name}: ${message}`, style, data || '')
  },

  info: (message, data = null) => {
    const style = `color: ${LOG_LEVELS.INFO.color}; font-weight: bold;`
    console.log(`%c[${module}] ${LOG_LEVELS.INFO.name}: ${message}`, style, data || '')
  },

  success: (message, data = null) => {
    const style = `color: ${LOG_LEVELS.SUCCESS.color}; font-weight: bold;`
    console.log(`%c[${module}] ${LOG_LEVELS.SUCCESS.name}: ${message}`, style, data || '')
  },

  warn: (message, data = null) => {
    const style = `color: ${LOG_LEVELS.WARN.color}; font-weight: bold;`
    console.warn(`%c[${module}] ${LOG_LEVELS.WARN.name}: ${message}`, style, data || '')
  },

  error: (message, error = null) => {
    const style = `color: ${LOG_LEVELS.ERROR.color}; font-weight: bold;`
    console.error(`%c[${module}] ${LOG_LEVELS.ERROR.name}: ${message}`, style, error || '')
  },
})

export default createLogger
