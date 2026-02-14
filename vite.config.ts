import { defineConfig } from 'vite'

export default defineConfig({
  root: './app/ui',
  build: {
    outDir: '../../ui',
    emptyOutDir: true,
  },
})
