/** @type {import('tailwindcss').Config} */
module.exports = {
  content: ["./frontend/src/**/*.{html,js,hs}"],
  theme: {
    extend: {
      fontFamily: {
        fira: ['Fira Code', 'Regular']
      },
      colors: {
        RED: '#ff6188',
        ORANGE: '#fc9867',
        YELLOW: '#ffd866',
        GREEN: '#a9dc76',
        BLUE: '#78dce8',
        PURPLE: '#ab9df2',
        WHITE: '#fdf9f3',
        BLACK: '#2c292d'
      }
    },
  },
  plugins: [],
}
